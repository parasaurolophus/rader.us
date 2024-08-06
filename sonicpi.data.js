import fs from 'node:fs'

export default {
    watch: ['./data/sonicpi/*.rb'],
    load(watchedFiles) {
        return watchedFiles.map((file) => {
            let path = file.split('/')
            return {
                file: path[path.length-1],
                contents: fs.readFileSync(file, 'utf-8')
            }
        })
    }
}
