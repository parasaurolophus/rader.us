<script setup>
import { VPTeamMembers } from 'vitepress/theme'

const members = [
    {
        avatar: 'https://github.com/parasaurolophus.png',
        name: 'Kirk Rader',
        title: 'Creator',
        links: [
            { icon: 'github', link: 'https://github.com/parasaurolophus' },
            { icon: 'linkedin', link: 'https://www.linkedin.com/in/kirkrader/' },
            {
                icon: {
                    svg: '<svg role="img" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"><title>HyperFollow</title><rect style="fill:#AAAAAA;stroke:#AAAAAA" width="24" height="24" x="0" y="0"/></svg>',
                },
                link: 'https://hyperfollow.com/kirkrader'
            },
        ]
    },
]
</script>

# About

<VPTeamMembers size="small" :members="members" />

---

<audio controls loop autoplay muted>
    <source src="/BrokenOaths.ogg" controlsList="nodownload noremoteplayback">
</audio>

![](/lava.webp)
