import { bus } from '../core/bus.js';
import { ajaxClient } from '../core/ajax.js';
import { msgEvent, BUS_EVENTS } from '../core/types.js';

function debounce(fn, ms) {
  let t = null;
  return (...args) => {
    clearTimeout(t);
    t = setTimeout(() => fn(...args), ms);
  };
}

function escapeRegExp(s) {
  return String(s).replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function highlightTextFragment(text, q, { className = 'hl' } = {}) {
  const source = String(text ?? '');
  const query = String(q ?? '').trim();
  const frag = document.createDocumentFragment();

  if (!query) {
    frag.appendChild(document.createTextNode(source));
    return frag;
  }

  const re = new RegExp(escapeRegExp(query), 'ig');

  let last = 0;
  let m;

  while ((m = re.exec(source)) !== null) {
    const start = m.index;
    const end = start + m[0].length;

    if (start > last) frag.appendChild(document.createTextNode(source.slice(last, start)));

    const mark = document.createElement('mark');
    mark.className = className;
    mark.textContent = source.slice(start, end);
    frag.appendChild(mark);

    last = end;
    if (re.lastIndex === m.index) re.lastIndex++;
  }

  if (last < source.length) frag.appendChild(document.createTextNode(source.slice(last)));
  return frag;
}

class TitleSearchPage {
  constructor() {
    this.qEl = null;
    this.container = null;

    this.offset = 0;
    this.limit = 24;

    this.loading = false;
    this.allLoaded = false;

    this.q = '';
  }

  init() {
    this.qEl = document.getElementById('title-search-q');
    this.container = document.getElementById('title-search-results');
    if (!this.qEl || !this.container) return;

    this.qEl.addEventListener(
      'input',
      debounce(() => this.applyQuery((this.qEl.value || '').trim()), 250)
    );

    window.addEventListener('scroll', () => {
      if (this.loading || this.allLoaded) return;
      const nearBottom =
        window.innerHeight + window.pageYOffset >= document.body.offsetHeight - 160;
      if (nearBottom) this.loadMore();
    });

    bus.on(msgEvent('books:title_search'), ({ payload }) => {
      this.loading = false;

      if (!Array.isArray(payload) || payload.length === 0) {
        this.allLoaded = true;
        return;
      }

      for (const book of payload) {
        this.container.appendChild(this.renderBookCard(book));
      }

      this.offset += payload.length;
      if (payload.length < this.limit) this.allLoaded = true;
    });

    this.applyQuery((this.qEl.value || '').trim());
  }

  applyQuery(q) {
    this.q = q;

    this.offset = 0;
    this.loading = false;
    this.allLoaded = false;

    this.container.innerHTML = '';

    if (!this.q) return;

    this.loadMore();
  }

  loadMore() {
    if (this.loading || this.allLoaded) return;
    if (!this.q) return;

    this.loading = true;

    const params = new URLSearchParams();
    params.set('offset', String(this.offset));
    params.set('limit', String(this.limit));
    params.set('q', this.q);

    ajaxClient.request({
      path: `/book/search?${params.toString()}`,
      method: 'GET',
      type: 'books:title_search',
    });
  }

  renderBookCard(book) {
    const item = document.createElement('article');
    item.className = 'search-book';

    const card = document.createElement('div');
    card.className = 'search-book__card';
    card.dataset.bookId = String(book.id);

    card.addEventListener('mouseenter', () => {
      bus.emit(BUS_EVENTS.UI.BOOK.HOVER, { id: book.id, el: card });
    });

    card.addEventListener('mouseleave', () => {
      bus.emit(BUS_EVENTS.UI.BOOK.LEAVE, { id: book.id, el: card });
    });

    const img = document.createElement('img');
    img.className = 'search-book__img';
    img.src = book.cover;
    img.alt = `Обложка книги '${book.title}'`;

    const title = document.createElement('div');
    title.className = 'search-book__title';
    title.replaceChildren(highlightTextFragment(book.title, this.q, { className: 'hl' }));

    card.appendChild(img);
    card.appendChild(title);
    item.appendChild(card);

    return item;
  }
}

export const titleSearchPage = new TitleSearchPage();
