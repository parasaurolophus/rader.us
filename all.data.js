import fs from 'node:fs'
import { parse } from 'csv-parse/sync'

export default {
  watch: [
    './data/musicography/*.csv',
    './data/sonicpi/*.rb',
  ],
  load(watchedFiles) {
    return watchedFiles.map((absolutePath) => {
      let contents = fs.readFileSync(absolutePath, 'utf-8')
      let result = {
        file: absolutePath,
        contents: contents,
      }
      if (absolutePath.endsWith('.csv')) {
        result.contents = parse(contents, { columns: true, skip_empty_lines: true })
      }
      return result
    })
  }
}
