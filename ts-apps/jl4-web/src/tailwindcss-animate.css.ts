@theme inline {
  --animation-delay-0: 0s;
  --animation-delay-75: 75ms;
  --animation-delay-100: 0.1s;
  --animation-delay-150: 0.15s;
  --animation-delay-200: 0.2s;
  --animation-delay-300: 0.3s;
  --animation-delay-500: 0.5s;
  --animation-delay-700: 0.7s;
  --animation-delay-1000: 1s;

  --animation-repeat-0: 0;
  --animation-repeat-1: 1;
  --animation-repeat-infinite: infinite;

  --animation-direction-normal: normal;
  --animation-direction-reverse: reverse;
  --animation-direction-alternate: alternate;
  --animation-direction-alternate-reverse: alternate-reverse;

  --animation-fill-mode-none: none;
  --animation-fill-mode-forwards: forwards;
  --animation-fill-mode-backwards: backwards;
  --animation-fill-mode-both: both;

  --animate-in: var(--tw-duration, 150ms) var(--tw-ease, ease) enter;
  --animate-out: var(--tw-duration, 150ms) var(--tw-ease, ease) exit;

  --percentage-0: 0;
  --percentage-5: 0.05;
  --percentage-10: 0.1;
  --percentage-15: 0.15;
  --percentage-20: 0.2;
  --percentage-25: 0.25;
  --percentage-30: 0.3;
  --percentage-35: 0.35;
  --percentage-40: 0.4;
  --percentage-45: 0.45;
  --percentage-50: 0.5;
  --percentage-55: 0.55;
  --percentage-60: 0.6;
  --percentage-65: 0.65;
  --percentage-70: 0.7;
  --percentage-75: 0.75;
  --percentage-80: 0.8;
  --percentage-85: 0.85;
  --percentage-90: 0.9;
  --percentage-95: 0.95;
  --percentage-100: 1;

  @keyframes enter {
    from {
      opacity: var(--tw-enter-opacity, 1);
      transform: translate3d(
          var(--tw-enter-translate-x, 0),
          var(--tw-enter-translate-y, 0),
          0
        )
        scale3d(
          var(--tw-enter-scale, 1),
          var(--tw-enter-scale, 1),
          var(--tw-enter-scale, 1)
        )
        rotate(var(--tw-enter-rotate, 0));
    }
  }

  @keyframes exit {
    to {
      opacity: var(--tw-exit-opacity, 1);
      transform: translate3d(
          var(--tw-exit-translate-x, 0),
          var(--tw-exit-translate-y, 0),
          0
        )
        scale3d(
          var(--tw-exit-scale, 1),
          var(--tw-exit-scale, 1),
          var(--tw-exit-scale, 1)
        )
        rotate(var(--tw-exit-rotate, 0));
    }
  }
}

/*
 * Tailwind's default `duration` utility sets the `--tw-duration` variable, so
 * can set `animation-duration` directly in the animation definition in the
 * `@theme` section above. Same goes for the `animation-timing-function`, set
 * with `--tw-ease`.
 */

@utility delay-* {
  animation-delay: --value([duration]);
  animation-delay: calc(--value(integer) * 1ms);
  animation-delay: --value(--animation-delay- *);
}

@utility repeat-* {
  animation-iteration-count: --value(--animation-repeat- *, integer);
}

@utility direction-* {
  animation-direction: --value(--animation-direction- *);
}

@utility fill-mode-* {
  animation-fill-mode: --value(--animation-fill-mode- *);
}

@utility running {
  animation-play-state: running;
}
@utility paused {
  animation-play-state: paused;
}

@utility fade-in-* {
  --tw-enter-opacity: --value(--percentage- *);
}
@utility fade-out-* {
  --tw-exit-opacity: --value(--percentage- *);
}

@utility zoom-in-* {
  --tw-enter-scale: --value(--percentage- *);
}
@utility zoom-out-* {
  --tw-exit-scale: --value(--percentage- *);
}

@utility spin-in-* {
  --tw-enter-rotate: calc(--value(integer) * 1deg);
  --tw-enter-rotate: --value(--rotate- *, [angle]);
}
@utility spin-out-* {
  --tw-exit-rotate: calc(--value(integer) * 1deg);
  --tw-exit-rotate: --value(--rotate- *, [angle]);
}

@utility slide-in-from-top-* {
  --tw-enter-translate-y: calc(--value([percentage], [length]) * -1);
}
@utility slide-in-from-bottom-* {
  --tw-enter-translate-y: --value([percentage], [length]);
}
@utility slide-in-from-left-* {
  --tw-enter-translate-x: calc(--value([percentage], [length]) * -1);
}
@utility slide-in-from-right-* {
  --tw-enter-translate-x: --value([percentage], [length]);
}

@utility slide-out-to-top-* {
  --tw-exit-translate-y: calc(--value([percentage], [length]) * -1);
}
@utility slide-out-to-bottom-* {
  --tw-exit-translate-y: --value([percentage], [length]);
}
@utility slide-out-to-left-* {
  --tw-exit-translate-x: calc(--value([percentage], [length]) * -1);
}
@utility slide-out-to-right-* {
  --tw-exit-translate-x: --value([percentage], [length]);
}
