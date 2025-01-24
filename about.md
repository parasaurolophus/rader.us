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
