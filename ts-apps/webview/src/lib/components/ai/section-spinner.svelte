<script lang="ts">
  // Section-sign "spotlight trace" spinner, ported from the
  // legalese.github.io /console/logs page. A faint § outline is always
  // visible while a brighter segment continuously traces around it;
  // three gradient stops shimmer across the fill in staggered phase.
  let { size = 36 }: { size?: number } = $props()
  const id = $derived(`section-spinner-${size}`)
</script>

<span
  class="wrap"
  style:width="{size}px"
  style:height="{size}px"
  role="status"
  aria-label="Loading"
>
  <svg width={size} height={size} viewBox="0 0 72 72" overflow="visible">
    <style>
      @keyframes spotlight-run {
        0% {
          stroke-dashoffset: 0;
        }
        100% {
          stroke-dashoffset: -500;
        }
      }
      @keyframes shimmer-sweep {
        0% {
          stop-opacity: 0;
        }
        40% {
          stop-opacity: 0.35;
        }
        60% {
          stop-opacity: 0.35;
        }
        100% {
          stop-opacity: 0;
        }
      }
    </style>
    <defs>
      <linearGradient
        id="{id}-grad"
        x1="0"
        y1="0"
        x2="72"
        y2="0"
        gradientUnits="userSpaceOnUse"
      >
        <stop offset="0%" stop-color="#9ca3af" stop-opacity="0" />
        <stop
          class="shimmer shimmer-1"
          offset="30%"
          stop-color="#9ca3af"
          stop-opacity="0"
        />
        <stop
          class="shimmer shimmer-2"
          offset="50%"
          stop-color="#9ca3af"
          stop-opacity="0"
        />
        <stop
          class="shimmer shimmer-3"
          offset="70%"
          stop-color="#9ca3af"
          stop-opacity="0"
        />
        <stop offset="100%" stop-color="#9ca3af" stop-opacity="0" />
      </linearGradient>
    </defs>
    <text
      class="sect fill"
      x="50%"
      y="58%"
      text-anchor="middle"
      dominant-baseline="middle"
      transform="rotate(90, 36, 36)"
      fill="url(#{id}-grad)">§</text
    >
    <text
      class="sect bg"
      x="50%"
      y="58%"
      text-anchor="middle"
      dominant-baseline="middle"
      transform="rotate(90, 36, 36)">§</text
    >
    <text
      class="sect hl"
      x="50%"
      y="58%"
      text-anchor="middle"
      dominant-baseline="middle"
      transform="rotate(90, 36, 36)">§</text
    >
  </svg>
</span>

<style>
  .wrap {
    display: inline-flex;
    align-items: center;
    justify-content: center;
  }
  .sect {
    font-family: 'Merriweather', Georgia, serif;
    font-weight: 900;
    font-size: 3rem;
  }
  .bg {
    fill: none;
    stroke: #d1d5db;
    stroke-width: 0.7;
    stroke-linecap: round;
    stroke-linejoin: round;
  }
  .hl {
    fill: none;
    stroke: #9ca3af;
    stroke-width: 1;
    stroke-dasharray: 80 420;
    stroke-linecap: round;
    stroke-linejoin: round;
    animation: spotlight-run 2.5s linear infinite;
  }
  .fill {
    stroke: none;
  }
  .shimmer-1 {
    animation: shimmer-sweep 2.5s ease-in-out infinite;
  }
  .shimmer-2 {
    animation: shimmer-sweep 2.5s ease-in-out 0.4s infinite;
  }
  .shimmer-3 {
    animation: shimmer-sweep 2.5s ease-in-out 0.8s infinite;
  }
</style>
