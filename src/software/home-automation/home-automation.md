&copy; Copyright Kirk Rader 2023. All rights reserved.

# Home Automation

```mermaid
graph TB

    browser["Web Browser"]

    subgraph "Home LAN"

        huebridge["Philips Hue Bridge(s)"]
        huedevice["Philips Hue Lights\nand Accessories"]
        pvhub["PowerView Hub"]
        pvdevice["Hunter-Douglas Window Coverings"]

        subgraph nodered["Node-RED"]

            vue["Vue / Vuetify\nWeb App"]
            flows["Flows\nhttps://github.com/parasaurolophus/home-automation"]
            dnssd["@parasaurolophus/nodered-dnssd"]
            eventsource["@parasaurolophus/nodered-eventsource"]

            vue <-- "WebSocket" --> flows
            dnssd --> flows
            eventsource --> flows

        end

        flows -- HTTP --> pvhub
        flows -- "HTTPS" --> huebridge
        huebridge -- "SSE" --> eventsource
        huebridge -- "mDNS" --> dnssd
        huedevice <-- "Zigbee" --> huebridge
        pvhub <--> pvdevice

        click flows "https://github.com/parasaurolophus/home-automation" _blank
        click dnssd "https://flows.nodered.org/node/@parasaurolophus/node-red-dnssd" _blank
        click eventsource "https://flows.nodered.org/node/@parasaurolophus/node-red-eventsource" _blank
        click vue "https://github.com/parasaurolophus/home-automation/tree/main/dashboard" _blank

    end

    browser <-- "HTTPS /\nWebSocket" ----> vue
```

- <https://github.com/parasaurolophus/home-automation>
- <https://flows.nodered.org/node/@parasaurolophus/node-red-dnssd>
- <https://flows.nodered.org/node/@parasaurolophus/node-red-eventsource>

## Features

- Entirely self-contained [Node-RED](https://nodered.org) based application
  - **No clouds!!**
  - No external configuration outside of _~/.node-red/settings.js_
    - Does require configuration of host running _Node-RED_ and the home
      LAN router in order to support secure access to the dashboard from
      outside the home[<sup>*</sup>](#https)
  - No additional servers or services required other than those provided
    by the Hue and PowerView hubs
- Consolidated dashboard / web-based UI using
  [Vue](https://vuejs.org/)/[Vuetify](https://vuetifyjs.com/)
  - Requires separate build step after cloning repo from
    GitHub[<sup>**</sup>](#plainhtml5)
- Time and date based automation
  - Turn on and off lights based on local sunrise / sunset
  - Use special occasion lighting themes based on date
  - Open and close window coverings based on sun's local altitude and
    azimuth on any given day

---

<a id="https"></a> <sup>*</sup>If remote access is not a priority, just leave
out the HTTPS related configuration from _~/.node-red/settings.js_. Detailed
instructions for obtaining and installing certificates and configuring IP
reservations and port forwarding on routers are beyond the scope of this web page.

<a id="plainhtml5"></a> <sup>**</sup>An earlier version of this project
implemented the dashboard using pure HTML 5 / JavaScript for its dashboard which
did not have additional build steps. I admit it. I was led astray by the pretty
toggle switches.
