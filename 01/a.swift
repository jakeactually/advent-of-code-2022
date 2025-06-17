import Foundation

let text = try! String(contentsOfFile: "input.txt")
let chunks = text.components(separatedBy: "\n\n")

let intChunks = chunks.map { chunk in
    chunk
        .components(separatedBy: .newlines)
        .filter { !$0.isEmpty }
        .map { Int($0)! }
}

let greatest = intChunks.map({ $0.reduce(0, +) }).max()!
print(greatest)
