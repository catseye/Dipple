package main

import ("os";"bufio")

func main() {
    reader := bufio.NewReader(os.Stdin);
    for {
        string, error := reader.ReadString('\n');
        if error != nil || string == "quit\n" { break }
        os.Stdout.WriteString(string)
    }
}
