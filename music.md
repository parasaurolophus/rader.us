<script setup>
import { data } from './music.data.js'
</script>

# Music

## Musicography

<template v-for="(album) in data">
    <h3>{{ album[0].album }}</h3>
    <p>UPC {{ album[0].upc }}</p>
    <table>
        <tr>
            <th>Track</th>
            <th>Title</th>
            <th>ISRC</th>
        </tr>
        <tr v-for="(item) in album">
            <td>{{ item.track }}</td>
            <td>{{ item.title }}</td>
            <td>{{ item.isrc }}</td>
        </tr>
    </table>
</template>
