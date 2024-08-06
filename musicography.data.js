import fs from 'node:fs'
import { parse } from 'csv-parse/sync'

export default {
  watch: ['./data/musicography/*.csv'],
  load(watchedFiles) {
    return watchedFiles.map((file) => {
      return parse(fs.readFileSync(file, 'utf-8'), {
        columns: true,
        skip_empty_lines: true
      })
    })
  }
}
