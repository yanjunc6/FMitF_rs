package util

import (
	"encoding/json"
	"fmt"
	"os"
)

/*create file*/
func CreateFile(path string) {
	// detect if file exists
	var _, err = os.Stat(path)

	// create file if not exists
	if os.IsNotExist(err) {
		var file, err = os.Create(path)
		if isError(err) {
			return
		}
		defer file.Close()
	}
}

/* print errors*/
func isError(err error) bool {
	if err != nil {
		fmt.Println(err.Error())
	}

	return (err != nil)
}

func WriteFile(path string, p interface{}) {
	var file, err = os.Create(path)

	if isError(err) {
		return
	}
	defer file.Close()

	// write into file
	output, _ := json.Marshal(p)
	_, err = file.WriteString(fmt.Sprintf("%s", output))
	if isError(err) {
		return
	}

	// save changes
	err = file.Sync()
	if isError(err) {
		return
	}
}
