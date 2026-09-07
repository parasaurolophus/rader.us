<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <details v-for="year of years" name="musicography">
        <summary>
            {{ year[0][0][YEAR] }}
        </summary>
        <details v-for="album of year" name="album">
            <summary>
                {{ album[0][ALBUM] }}
            </summary>
            <div class="subtitle">
                UPC {{ album[0][UPC] }}
            </div>
            <table>
                <thead>
                    <tr>
                        <th>{{ headers[TRACK] }}</th>
                        <th>{{ headers[TITLE] }}</th>
                        <th>{{ headers[ISRC] }}</th>
                    </tr>
                </thead>
                <tbody>
                    <tr v-for="track of album">
                        <td>{{ track[TRACK] }}</td>
                        <td>{{ track[TITLE] }}</td>
                        <td>{{ track[ISRC] }}</td>
                    </tr>
                </tbody>
            </table>
        </details>
    </details>
</template>

<script setup>
import { onMounted, ref } from 'vue'
import data from '/assets/musicography.tsv?raw'

const RELEASE = 0
const YEAR = 1
const ALBUM = 2
const UPC = 3
const TITLE = 4
const TRACK = 5
const ISRC = 6

const headers = ref([])
const years = ref([])

function ensureAlbum(year, trackUpc) {

    for (let album of year) {

        const track = album[0]

        if (track[UPC] === trackUpc) {

            return album
        }
    }

    const album = []
    year.push(album)
    return album
}

function ensureYear(trackYear) {

    for (let year of years.value) {

        const album = year[0]
        const track = album[0]

        if (track[YEAR] === trackYear) {

            return year
        }
    }

    const year = []
    years.value.push(year)
    return year
}

onMounted(() => {

    const lines = data.split('\r\n')

    lines.forEach(line => {

        try {

            const track = line.split('\t')

            if (track[RELEASE] === 'release') {

                headers.value = track
                return
            }

            if (!(track[RELEASE] && track[UPC])) {

                return
            }

            if (!Array.isArray(track)) {

                throw `expected ${track} to be an array`
            }

            if (track.length !== 7) {

                throw `expected length of ${track} to be 7, got ${track.length}`
            }

            track[RELEASE] = Number.parseInt(track[RELEASE])
            track[YEAR] = Number.parseInt(track[YEAR])
            track[TRACK] = Number.parseInt(track[TRACK])

            const year = ensureYear(track[YEAR])
            const album = ensureAlbum(year, track[UPC])

            album.push(track)

        } catch (error) {

            console.error(error)
        }
    })
})
</script>