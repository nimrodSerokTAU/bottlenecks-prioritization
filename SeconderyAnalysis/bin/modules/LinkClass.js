const fs = require('fs');
const mongoose = require('mongoose');
const hullCalc = require('hull.js');
const random = require('random');


const utils = require('./utils');
const linkSchema = require('../schemas/link.schema');

exports.fillLinks = async function(workingDirPath, numberOfIterations){
    const links = new Links();

    for(let i = 0; i < numberOfIterations; i++){
        const filePath = `${workingDirPath}/LoadClusters_${i}.txt`;
        const fileRes = fs.readFileSync(filePath, {encoding: 'utf8'});
        links.csvJSONTOLinks(fileRes, i);
    }
    links.fillLinksData();
    return await links.saveLinks();
};

class Link {
    constructor(linkId, origin, destination) {
        this.linkId = Number.parseInt(linkId);
        this.origin = utils.stringToPoint(origin);
        this.destination = utils.stringToPoint(destination);
        this.bottleNecks = {};
        this.maxDist = 0;
        this.medianDist = 0;
        this.maxSimDist = 0;
        this.medianSimDist = 0;
        this.breakdown = [];
        this.sumCostOfTrees = [0];
        this.sumOfOwnCost = [0];
        this.upTo90Count = 0;
        this.upTo100Count = 0;
        this.lastIter = -1;
        this.myBottleNeckSLocation = [];
    }

    addBottleneck(linkKey, treeCost, branchCost, iteration, botKey) {
        if(!this.bottleNecks[botKey]){
            this.bottleNecks[botKey] = 0;
        }
        this.bottleNecks[botKey]++;
        if(this.lastIter + 1 === iteration){ // same tree:
            this.sumCostOfTrees[this.sumCostOfTrees.length - 1] = treeCost;
            this.sumOfOwnCost[this.sumOfOwnCost.length - 1] = branchCost;
        }
        else{ // new tree:
            this.sumCostOfTrees.push(treeCost);
            this.sumOfOwnCost.push(branchCost);
        }
        this.lastIter = iteration;
    }

    fillBotData(allLinks, botResForSim, botResForSimAll){
        const botNum = Object.keys(this.bottleNecks).length;
        const botNumKey = `BN${botNum}`;
        if(!botResForSim[botNumKey]){
            botResForSim[botNumKey] = {};
        }
        for(const botKey of Object.keys(this.bottleNecks)){
            this.myBottleNeckSLocation.push(allLinks[botKey].destination.coordinates);
            botResForSim[botNumKey][`B${botKey}`] = allLinks[botKey].destination.coordinates;
            if(!botResForSimAll[`B${botKey}`]){
                botResForSimAll[`B${botKey}`] = allLinks[botKey].destination.coordinates;
            }
        }
    }

    getBottlenecksDistance(bottleNeckSLocation){
        if(bottleNeckSLocation.length === 1){
            return {maxDist:0, medianDist:0}
        }
        const convexHull = hullCalc(bottleNeckSLocation, 100);
        const distances = [];
        for (let i = 0; i < convexHull.length - 1; i++){
            for (let j = i + 1; j < convexHull.length; j++){
                distances.push (utils.calcDistance(convexHull[i], convexHull[j]));
            }
        }
        distances.sort((a, b) => b - a);
        const maxDist = distances[0] > 1 ? distances[0] : 0;
        const medianDistValue = distances[Math.floor(distances.length / 2)];
        const medianDist = medianDistValue > 1 ? medianDistValue : 0;
        return {maxDist, medianDist};
    }

    fillSimulationData(){
        const distRes = this.getBottlenecksDistance(this.myBottleNeckSLocation);
        this.maxDist = distRes.maxDist;
        this.medianDist = distRes.medianDist;
        this.sumCostOfTrees = this.sumCostOfTrees.reduce((a, b) => a + b, 0);
        this.sumOfOwnCost = this.sumOfOwnCost.reduce((a, b) => a + b, 0);
    }

    simulateLinksBot(relevantBottlenecks){
        const myRandomBot =[];
        for(let i=0; i < this.breakdown.length; i++){
            const randomIndex = random.int(0, relevantBottlenecks.length - 1);
            myRandomBot.push(relevantBottlenecks[randomIndex]);
            relevantBottlenecks.splice(randomIndex,1);
        }
        const distRes = this.getBottlenecksDistance(myRandomBot);
        this.maxSimDist = distRes.maxDist;
        this.medianSimDist = distRes.medianDist;
    }

    calcUpTo90(){
        const values = Object.values(this.bottleNecks);
        this.breakdown = values.sort((a, b) => b - a);
        const sumAll = this.breakdown.reduce((a, b) => a + b, 0);
        this.upTo100Count = this.breakdown.length;
        let upTo90Count = 0;
        let upTo90sum = 0;
        while (upTo90sum < 0.9 * sumAll){
            upTo90sum += this.breakdown[upTo90Count];
            upTo90Count++;
        }
        this.upTo90Count = upTo90Count;
    }

    saveObj(){
        return {
            linkId: this.linkId,
            origin: this.origin,
            destination: this.destination,
            bottleNecks: this.bottleNecks,
            maxDist: this.maxDist,
            medianDist: this.medianDist,
            maxSimDist: this.maxSimDist,
            medianSimDist: this.medianSimDist,
            sumCostOfTrees: this.sumCostOfTrees,
            sumOfOwnCost: this.sumOfOwnCost,
            upTo90Count: this.upTo90Count,
            upTo100Count: this.upTo100Count,
        }
    }
}

class Links {
    constructor() {
        this.allLinks = {};
        this.botResForSim = {};
        this.botResForSimAll = {};
    }

    csvJSONTOLinks(csv, iteration){
        const lines=csv.split("\n");
        const headers = lines[0].split(",");
        for(let i = 1; i < lines.length; i++){
            const currentLine = lines[i].split(",");
            if(currentLine.length <= 1){
                return; //JSON
            }
            const linkObj = {};
            for(let j = 0; j < headers.length; j++){
                linkObj[headers[j]] = currentLine[j];
            }
            const linkKey = `L${linkObj.linkId}`;
            const botKey = `L${linkObj.trunk}`;

            if(!this.allLinks[linkKey]){
                this.allLinks[linkKey] = new Link(linkObj.linkId, linkObj.fromLocation, linkObj.toLocation);
            }
            const thisLink = this.allLinks[linkKey];
            const treeCost = Number.parseInt(linkObj.wTCost);
            const branchCost = Number.parseInt(linkObj[' branchCost']);
            thisLink.addBottleneck(linkKey, treeCost, branchCost, iteration, botKey);
        }
    }

    fillLinksData(addition){

        for(const link of Object.values(this.allLinks)) {
            link.calcUpTo90();
            link.fillBotData(this.allLinks, this.botResForSim, this.botResForSimAll);
        }

        for(const link of Object.values(this.allLinks)) {
            link.fillSimulationData();
            link.simulateLinksBot([...Object.values(this.botResForSimAll)]);
        }
    }

    async saveLinks(){
        const LinksModel = mongoose.model('pb-links', linkSchema);
        const res = (Object.values(this.allLinks)).map(x=>(x.saveObj()));
        LinksModel.collection.insertMany(res)
            .then(function(docs) {
                Promise.resolve();
            })
            .catch(function(err) {
                Promise.reject();
            });
    }
}
