export const msgEvent = (t) => `msg:${t}`;

export const BUS_EVENTS = {
  AJAX: {
    RESPONSE: 'ajax:response',
  },

  UI: {
    BOOK: {
      CLICK: 'ui:book:click',
      HOVER: 'ui:book:hover',
    },
    NAV: {
      GOTO: 'ui:nav:goto',
    },
    CATALOG: {
      FILTER_CHANGED: 'ui:catalog:filter_changed',
    },
  },

  BOOK: {
    LOAD_DETAILS:   'book:load_details',
    DETAILS_LOADED: 'book:details_loaded',
  },
};
