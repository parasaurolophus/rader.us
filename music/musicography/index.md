<script setup>
import { data } from '/all.data.js'
</script>

<style scoped>
    .upc {
        font-style: italic;
        font-size: small;
    }
</style>

# Musicography

<template v-for="datum in data">
    <template v-for="album in datum">
        <h2>{{ album[0].album}}</h2>
        <div class="upc">{{ album[0].upc }}</div>
        <div>{{ album[0].year }}</div>
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
