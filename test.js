// Make the DIV element draggable:
document.querySelectorAll('.dragable-window').forEach(window => {
    let offsetX, offsetY;

    window.addEventListener('mousedown', (e) => {
        offsetX = e.clientX - window.getBoundingClientRect().left;
        offsetY = e.clientY - window.getBoundingClientRect().top;

        window.style.cursor = 'grabbing';
        
        const mouseMoveHandler = (e) => {
            window.style.left = `${e.clientX - offsetX}px`;
            window.style.top = `${e.clientY - offsetY}px`;
        };

        const mouseUpHandler = () => {
            window.style.cursor = 'move';
            document.removeEventListener('mousemove', mouseMoveHandler);
            document.removeEventListener('mouseup', mouseUpHandler);
        };

        document.addEventListener('mousemove', mouseMoveHandler);
        document.addEventListener('mouseup', mouseUpHandler);
    });
});


function closeWindow(id){
  document.getElementById(id).style.display = 'none';
  }

function openWindow(id){
  document.getElementById(id).style.display = '';
  }