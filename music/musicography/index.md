<script setup>
import { data } from '/all.data.js'
</script>

# Musicography

<template v-for="datum in data">
    <template v-for="album in datum">
        <h2 :id="album[0].upc">
            <a :href="'#' + album[0].upc">
                {{ album[0].album}}
            </a>
        </h2>
        <div>UPC: <i>{{ album[0].upc }}</i></div>
        <div>Year: <i>{{ album[0].year }}</i></div>
        <table>
            <tr>
                <th>Track</th>
                <th>Title</th>
                <th>ISRC</th>
            </tr>
            <template v-for="track in album">
                <tr>
                    <td>{{ track.track }}</td>
                    <td>{{ track.title }}</td>
                    <td>{{ track.isrc }}</td>
                </tr>
            </template>
        </table>
    </template>
</template>
