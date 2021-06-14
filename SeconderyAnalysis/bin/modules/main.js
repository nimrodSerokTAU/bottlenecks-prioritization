const mongoose = require('mongoose');

const links = require('./LinkClass');
const trunks = require('./TrunkClass');

const workingDirPath = "D:/PB/workingDir/loadCluster";
const numberOfIterations = 480;

exports.myMain = async function(){
    await mongoose.connect('mongodb://localhost:27017/pb', {useNewUrlParser: true, useUnifiedTopology: true });
    console.log('Start');
    //
    await links.fillLinks(workingDirPath, numberOfIterations);
    // await trunks.fillTrunks(workingDirPath, numberOfIterations);

    console.log('End');
};




