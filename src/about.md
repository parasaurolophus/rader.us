# About

<VPTeamMembers size="small" :members="members" />
<script setup>
import { VPTeamMembers } from 'vitepress/theme'

const members = [
    {
        avatar: 'https://github.com/parasaurolophus.png',
        name: 'Kirk Rader',
        title: 'Creator',
        links: [
            { icon: 'github', link: 'https://github.com/parasaurolophus' },
            {
                icon: {
                    svg: '<svg role="img" viewBox="0 0 512 512" xmlns="http://www.w3.org/2000/svg"><path d="M286.197 189.663L109 69.4539V391.791L286.197 512V189.663Z" fill="gray"/><path d="M226.983 84.1461V0L403.289 121.099V443.882L338.732 399.805V162.059L226.983 84.1461Z" fill="gray"/></svg>',
                },
                link: 'https://hyperfollow.com/kirkrader'
            },
        ]
    },
]
</script>
