/** Tools extend the simple Result/Either monad to include better handlers and granularity
 *
 * THINK: Do we want to add a warning accumulator, eg Ok|Warn|Error instead of just Result?
 */

/** The severity of the error.

  Sometimes an error is really just something is in an incorrect state to run on the railroad.
 */
type errorLevel =
  | Info
  | Warn
  | Err
  | Panic;

type t('a) = {
  /** Severity of the given error */
  level: errorLevel,
  /** A display message */
  msg: string,
  /** The user's custom error type */
  errorType: 'a,
  /** Accumulator for multiple/nested errors in a s */
  children: array(t('a)),
};

let toStr = (exn: t('a)) =>
  Printf.sprintf(
    "[%s] %s",
    switch (exn.level) {
    | Info => "Info"
    | Warn => "Warn"
    | Err => "Error"
    | Panic => "Panic"
    },
    exn.msg,
  );

/**  Aliases for Belt.Result so we can abstract it away, since mixing custom functions could get confusing
 to future devs. Alse, we don't need to remember to put the open at the top */
let ok = (value: 'a) => Belt.Result.Ok(value);
let err = (~level=Err, ~msg="", ~children=[||], errorType: 'a): Belt.Result.t('b, t('a)) => {
  Belt.Result.Error({level, msg, errorType, children});
};

/** Get the value or else raise the error */
let getExn = (result: Belt.Result.t('a, 'b)) =>
  switch (result) {
  | Ok(x) => x
  | Error(err) =>
    let msg = err->toStr;
    Js.log(msg);
    Js.Exn.raiseError(msg);
  };

let ifOk = (func, item) =>
  switch (item) {
  | Belt.Result.Error(err) => Belt.Result.Error(err)
  | Ok(item) => func(item)
  };

/** Merge a list of Eeyo Results into a single list of values
 *
 * FIXME: Decide how to handle item.errorType for multiple errors
 */
let flattenExn = (itemList: list(Belt.Result.t('a, t('b)))): Belt.Result.t(list('a), t('b)) => {
  itemList
  |> List.fold_left(
       (acc, item) =>
         switch (acc: Belt.Result.t(list('a), t('b)), item) {
         | (Belt.Result.Ok(acc), Belt.Result.Ok(item)) => Ok(List.append([item], acc))
         | (Belt.Result.Error(acc), Belt.Result.Ok(_)) => Belt.Result.Error(acc)
         | (Belt.Result.Ok(_), Belt.Result.Error(err)) => Belt.Result.Error(err)
         | (Belt.Result.Error(acc), Belt.Result.Error(item)) =>
           err(
             // Always elevate to the highest level of exception
             ~level=
               switch (acc.level, item.level) {
               | (Panic, _)
               | (_, Panic) => Panic
               | (Err, _)
               | (_, Err) => Err
               | (Warn, _)
               | (_, Warn) => Warn
               | (Info, Info) => Info
               },
             ~msg=Printf.sprintf("%s\n%s", acc.msg, item |> toStr),
             item.errorType,
           )
         },
       Ok([]),
     );
};

let mapErr = (func, item) =>
  switch (item) {
  | Ok(value) => func(value)
  | error => error
  };

let removeExn = (itemList: list(Belt.Result.t('a, t('b)))): list(Belt.Result.t('a, t('b))) =>
  itemList |> List.filter(Belt.Result.isOk);

let getExns = (itemList: list(Belt.Result.t('a, t('b)))): list(Belt.Result.t('a, t('b))) =>
  itemList |> List.filter(Belt.Result.isError);

/** I've adopted the railway method during my years of F# and find the pattern allows me to sipmlify things
 https://fsharpforfunandprofit.com/rop/ */
let kleisliComposition =
    (func: 'a => Belt.Result.t('b, t('c)), item: Belt.Result.t('a, t('c))): Belt.Result.t('b, t('c)) =>
  switch (item) {
  | Ok(value) => func(value)
  | Error(err) => Error(err)
  };

// Just adding a simple shortcut, since operator overloads don't seem to work for compositions
// This really should be ">=>" instead of "|> kC"
let kC = kleisliComposition;
