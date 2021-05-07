"use strict";

module.exports = {
  purge: ["./src/**/*.njk", "./src/**/*.js"],
  darkMode: false, // or 'media' or 'class'
  theme: {
    colors: {
      transparent: "transparent",
      current: "currentColor",
      // FIXME: Marlowe run uses "#283346" but the website uses this, unify
      black: "#273245",
      // FIXME: Marlowe run uses "#eeeeee" but the website uses this, unify
      lightgray: "#f6f9fc",
      gray: "#dfdfdf",
      // FIXME: Marlowe run uses "#00a551" but the website uses this, unify
      green: "#00e39c",
      lightgreen: "#00e872",
      darkgray: "#b7b7b7",
      overlay: "rgba(10,10,10,0.4)",
      white: "#ffffff",
      purple: "#4700c3",
      purple: "#4700c3",
      lightpurple: "#8701fc",
      grayblue: "#f5f9fc",
      red: "#e04b4c",
    },
    fontFamily: {
      barlowe: ["barlowe", "sans-serif"],
      comfortaa: ["comfortaa", "sans-serif"],
    },
    fontSize: {
      xs: "12px",
      sm: "14px",
      base: "16px",
      lg: "18px",
      xl: "22px",
      "2xl": "24px",
      "3xl": "36px",
      "5xl": "68px",
    },
    borderRadius: {
      sm: "5px",
      DEFAULT: "10px",
      lg: "25px",
      full: "9999px",
    },

    boxShadow: {
      none: "none",
      sm: "0 4px 6px -1px rgba(0,0,0,0.1), 0 2px 4px -1px rgba(0,0,0,0.06)",
      DEFAULT: "0 10px 15px -3px rgba(0,0,0,0.1), 0 4px 6px -2px rgba(0,0,0,0.05)",
      lg: "0 20px 25px -5px rgba(0,0,0,0.2), 0 10px 10px -5px rgba(0,0,0,0.04)",
      xl: "0 25px 50px -12px rgba(0,0,0,0.25)",
      deep: "0 2.5px 5px 0 rgba(0, 0, 0, 0.22)",
    },
    extend: {
      spacing: {
        "5pc": "5%",
      },
      padding: {
        /* This value was obtained from a "hack" to make the div occupy the height of
           its background image
           https://stackoverflow.com/questions/600743/how-to-get-div-height-to-auto-adjust-to-background-size
           */
        /* (img-height / img-width * container-width) */
        /* (927 / 1440 * 100) */
        "main-bg": "64.37%",
      },
      backgroundImage: (theme) => ({
        main: "url('/static/img/woman-using-cellphone.jpg')",
      }),
      borderWidth: {
        3: "3px",
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [],
  corePlugins: {
    container: false,
    space: true,
    divideWidth: false,
    divideColor: false,
    divideStyle: false,
    divideOpacity: false,
    accessibility: false,
    appearance: false,
    backgroundAttachment: false,
    backgroundClip: false,
    backgroundColor: true,
    backgroundImage: true,
    gradientColorStops: true,
    backgroundOpacity: false,
    backgroundPosition: false,
    backgroundRepeat: true,
    backgroundSize: true,
    borderCollapse: false,
    borderColor: true,
    borderOpacity: false,
    borderRadius: true,
    borderStyle: false,
    borderWidth: true,
    boxSizing: false,
    cursor: true,
    display: true,
    flexDirection: true,
    flexWrap: false,
    placeItems: false,
    placeContent: false,
    placeSelf: false,
    alignItems: true,
    alignContent: true,
    alignSelf: true,
    justifyItems: false,
    justifyContent: true,
    justifySelf: false,
    flex: true,
    flexGrow: true,
    flexShrink: true,
    order: false,
    float: true,
    clear: false,
    fontFamily: true,
    fontWeight: true,
    height: true,
    lineHeight: true,
    listStylePosition: false,
    listStyleType: false,
    maxHeight: true,
    maxWidth: true,
    minHeight: true,
    minWidth: true,
    objectFit: false,
    objectPosition: false,
    opacity: true,
    outline: true,
    overflow: true,
    overscrollBehavior: false,
    placeholderColor: false,
    placeholderOpacity: false,
    pointerEvents: true,
    position: true,
    inset: true,
    resize: false,
    boxShadow: true,
    ringWidth: true,
    ringOffsetColor: false,
    ringOffsetWidth: false,
    ringColor: true,
    ringOpacity: false,
    fill: false,
    stroke: false,
    strokeWidth: false,
    tableLayout: false,
    textAlign: true,
    textOpacity: false,
    textOverflow: true,
    fontStyle: false,
    textTransform: true,
    textDecoration: true,
    fontSmoothing: false,
    fontVariantNumeric: false,
    letterSpacing: false,
    userSelect: false,
    verticalAlign: false,
    visibility: true,
    whitespace: true,
    wordBreak: false,
    width: true,
    zIndex: true,
    gap: true,
    gridAutoFlow: false,
    gridTemplateColumns: true,
    gridAutoColumns: false,
    gridColumn: false,
    gridColumnStart: false,
    gridColumnEnd: false,
    gridTemplateRows: true,
    gridAutoRows: false,
    gridRow: false,
    gridRowStart: false,
    gridRowEnd: false,
    transform: true,
    transformOrigin: true,
    scale: true,
    rotate: false,
    translate: true,
    skew: false,
    transitionProperty: true,
    transitionTimingFunction: true,
    transitionDuration: true,
    transitionDelay: false,
    animation: true,
  },
};
