// https://github.com/microsoft/vscode-languageserver-node/blob/release/protocol/3.16.0-next.10/protocol/src/common/protocol.ts

module Message = {
  type t = {jsonrpc: string};
};

// module Cancellation = {
//   module Cancellation = {
//     type t('a) = {
//       isCancellationRequested: bool,
//       onCancellationRequested: ;
//     }
//   };

//   // https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/jsonrpc/src/common/cancellation.ts#L91-L94
//   module AbstractCancellationTokenSource = {
//     type t = {token: Cancellation.t};
//   };
// };

// // https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/jsonrpc/src/common/connection.ts#L310-L325
// module CancellationReceiverStrategy = {
//   type t = {
//     createCancellationTokenSource: string => AbstractCancellationTokenSource.t,
//     dispose: option(unit => unit),
//   };
// };
// // https://github.com/microsoft/vscode-languageserver-node/blob/875bf6c1a4313a5245691c063607e5f031e35308/jsonrpc/src/common/connection.ts#L346-L360
// module CancellationStrategy = {
//   type t = {
//     receiver: CancellationReceiverStrategy.t,
//     sender: CancellationSenderStrategy.t,
//   };
// };

// export interface CancellationStrategy {
// 	receiver: CancellationReceiverStrategy;
// 	sender: CancellationSenderStrategy;
