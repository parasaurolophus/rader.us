import fs from 'node:fs'
import { parse } from 'csv-parse/sync'

export default {
    watch: [
        './data/musicography.csv',
    ],
    load(watchedFiles) {
        return watchedFiles.map((absolutePath) => {
            const contents = fs.readFileSync(absolutePath, 'utf-8')
            const csv = parse(contents, { columns: true, skip_empty_lines: true })
            const albums = []
            let album = []
            for (let row of csv) {
                if (!row.upc) {
                    continue
                }
                if (album.length > 0 && row.upc != album[0].upc) {
                    albums.push(album)
                    album = []
                }
                album.push(row)
            }
            if (album.length > 0) {
                albums.push(album)
            }
            return albums
        })
    }
}
