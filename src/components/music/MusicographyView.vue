<!-- Copyright (c) Kirk Rader 2026 -->

<template>
    <DetailsComponent v-for="year of years">
        <template #summary>
            {{ year[0][0][1] }}
        </template>
        <DetailsComponent v-for="album of year">
            <template #summary>
                {{ album[0][2] }}
            </template>
            <table>
                <thead>
                    <tr>
                        <th v-for="header of headers">
                            {{ header }}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    <tr v-for="track of album">
                        <td v-for="column of track">
                            {{ column }}
                        </td>
                    </tr>
                </tbody>
            </table>
        </DetailsComponent>
    </DetailsComponent>
</template>

<style scoped>
th,
td {
    padding: 0.25em;
}

tr,
th,
td {
    border-radius: 0;
}

thead>tr,
tr:nth-child(even) {
    background-color: var(--highlight);
}
</style>

<script setup>
import { onMounted, ref } from 'vue'
import data from '/assets/musicography.tsv?raw'
import DetailsComponent from '@/components/DetailsComponent.vue'

const headers = ref([])
const years = ref([])

function ensureAlbum(year, trackUpc) {

    for (let album of year) {

        const track = album[0]

        if (track[3] === trackUpc) {

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

        if (track[1] === trackYear) {

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

            if (track[0] === 'release') {

                headers.value = track
                return
            }

            if (!(track[0] && track[3])) {

                return
            }

            if (!Array.isArray(track)) {

                throw `expected ${track} to be an array`
            }

            if (track.length !== 7) {

                throw `expected length of ${track} to be 7, got ${track.length}`
            }

            track[0] = Number.parseInt(track[0])
            track[1] = Number.parseInt(track[1])
            track[5] = Number.parseInt(track[5])

            const year = ensureYear(track[1])
            const album = ensureAlbum(year, track[3])

            album.push(track)

        } catch (error) {

            console.error(error)
        }
    })
})
</script>