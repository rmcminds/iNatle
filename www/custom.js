// custom.js

const letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('');
const all_key_ids = [...letters, 'Enter', 'Back'];

document.addEventListener('keydown', function(e) {
  let key = e.code.replace(/^Key/, '');
  if (letters.includes(key)) {
    document.getElementById(key).click();
  } else if (key === 'Enter') {
    document.getElementById('Enter').click();
  } else if (key === 'Backspace') {
    document.getElementById('Back').click();
  }
});

document.addEventListener('touchstart', function(e) {
  if (all_key_ids.includes(e.target.id)) {
    e.target.click();
    e.target.style.pointerEvents = 'none';
    e.preventDefault();
    in_button_touch = true;
  }
});

document.addEventListener('touchend', function(e) {
  all_key_ids.forEach(id => {
    document.getElementById(id).style.pointerEvents = null;
  });
  if (in_button_touch) {
      if (all_key_ids.includes(e.target.id)) {
          // Disable text selection and triggering of click event.
          e.preventDefault();
      }
    in_button_touch = false;
  }
});
