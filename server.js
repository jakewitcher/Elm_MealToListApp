const express = require('express');
const app = express();
const port = process.env.port || 3000;

app.set('view engine', 'ejs');
app.use(express.static(__dirname + '/public'));

app.get('/', (req, res) =>
    res.render('index'));

app.listen(port, () =>
    console.log(`The server has started at port ${port}`));