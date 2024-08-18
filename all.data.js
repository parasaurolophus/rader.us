import fs from 'node:fs'
import { parse } from 'csv-parse/sync'

export default {
  watch: [
    './data/musicography.csv',
    './data/sonicpi/*.rb',
  ],
  load(watchedFiles) {
    return watchedFiles.map((absolutePath) => {
      const contents = fs.readFileSync(absolutePath, 'utf-8')
      const result = {
        file: absolutePath,
        contents: contents,
      }
      if (absolutePath.endsWith('/musicography.csv')) {
        const csv = parse(contents, { columns: true, skip_empty_lines: true })
        const albums = []
        let album = null
        let row = null
        for (row of csv) {
          if (!row.upc) {
            continue
          }
          if (!album || album.length == 0 || row.upc != album[0].upc) {
            if (album) {
              albums.push(album)
            }
            album = []
          }
          album.push(row)
        }
        if (album) {
          albums.push(album)
        }
        result.contents = albums
      }
      return result
    })
  }
}
