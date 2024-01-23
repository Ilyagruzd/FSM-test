open System
type tcpState = CLOSED | LISTEN | SYN_SENT | SYN_RCVD | ESTABLISHED | CLOSE_WAIT | LAST_ACK | FIN_WAIT_1 | FIN_WAIT_2 | CLOSING | TIME_WAIT

type tcpEvent = APP_PASSIVE_OPEN | APP_ACTIVE_OPEN | APP_SEND | APP_CLOSE | APP_TIMEOUT | RCV_SYN | RCV_ACK | RCV_SYN_ACK | RCV_FIN | RCV_FIN_ACK

let transitionTable = [|
    (CLOSED, APP_PASSIVE_OPEN, LISTEN);
    (CLOSED, APP_ACTIVE_OPEN, SYN_RCVD);
    (LISTEN, RCV_SYN, SYN_RCVD);
    (LISTEN, APP_SEND, SYN_SENT);
    (LISTEN, APP_CLOSE, CLOSED);
    (SYN_RCVD, APP_CLOSE, FIN_WAIT_1);
    (SYN_RCVD, RCV_ACK, ESTABLISHED);
    (SYN_SENT, RCV_SYN, SYN_RCVD);
    (SYN_SENT, RCV_SYN_ACK, ESTABLISHED);
    (SYN_SENT, APP_CLOSE, CLOSED);
    (ESTABLISHED, APP_CLOSE, FIN_WAIT_1);
    (ESTABLISHED, RCV_FIN, CLOSE_WAIT);
    (FIN_WAIT_1, RCV_FIN, CLOSING);
    (FIN_WAIT_1, RCV_FIN_ACK, TIME_WAIT);
    (FIN_WAIT_1, RCV_ACK, FIN_WAIT_2);
    (CLOSING, RCV_ACK, TIME_WAIT);
    (FIN_WAIT_2, RCV_FIN, TIME_WAIT);
    (TIME_WAIT, APP_TIMEOUT, CLOSED);
    (CLOSE_WAIT, APP_CLOSE, LAST_ACK);
    (LAST_ACK, RCV_ACK, CLOSED)
|]

let readEvents () =
    printfn "Введите список событый через запятую"
    let events = Console.ReadLine()
    events.Split(",", StringSplitOptions.RemoveEmptyEntries)

let processEvent currentState event =
    match Array.tryFind (fun (prevState, e, _) -> prevState = currentState && e.ToString() = event) transitionTable with
    | Some (_, _, nextState) -> nextState
    | None -> failwith "ERROR"

let eventsArray = readEvents ()
                  |> Array.map(fun event -> event.Trim())
                  |> Array.toList

let rec processEvents currentState events =
    match events with
    | [] -> currentState
    | event :: restEvents -> processEvents (processEvent currentState event) restEvents
    

try
    let result = processEvents CLOSED eventsArray
    printf $"{result}"
with
| ex -> printfn $"{ex.Message}"

