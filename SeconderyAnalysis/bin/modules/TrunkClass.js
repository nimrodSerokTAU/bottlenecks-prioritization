const fs = require('fs');
const mongoose = require('mongoose');
const trunkSchema = require('../schemas/trunk.schema');

exports.fillTrunks = async function(workingDirPath, numberOfIterations){
    const trunks = new Trunks();
    for(let i = 0; i < numberOfIterations; i++){
        const filePath = `${workingDirPath}/LoadClusters_${i}.txt`;
        const fileRes = fs.readFileSync(filePath, {encoding: 'utf8'});
        trunks.csvJSONTOTrunks(fileRes, i);
    }
    return await trunks.saveTrunks();
};

class Trunk {
    constructor(trunkId, loadedFor, iteration) {
        this.trunkId = trunkId;
        this.loadedFor = loadedFor;
        this.startIteration = iteration;
        this.wtCost = 0;
        this.key = `${trunkId}-${iteration}`;
        this.time = 0;
    }

    generationCalculate(formerBranches, trunksBefore2, allCurrentTrunksById, formerBranchesTrunksMap){
        if (formerBranches.indexOf(this.trunkId) < 0 || trunksBefore2.indexOf(this.trunkId) >= 0) {
            this.generation = 0;
            this.otherCosts = [];
            this.otherDurations = []
        } else {
            const formerTrunkId = formerBranchesTrunksMap[`B${this.trunkId}`];
            this.generation = allCurrentTrunksById[formerTrunkId].generation + 1;
            this.otherCosts = [...allCurrentTrunksById[formerTrunkId].otherCosts, allCurrentTrunksById[formerTrunkId].wtCost];
            this.otherDurations = [...allCurrentTrunksById[formerTrunkId].otherDurations, allCurrentTrunksById[formerTrunkId].time];
        }
    }
}

class TrunkSummary {
    constructor(trunk){
        this.generation = trunk.generation;
        this.otherCosts = [...trunk.otherCosts, trunk.wtCost];
        this.otherDurations = [...trunk.otherDurations, trunk.time];
        this.maxCost =  Math.max(...this.otherCosts);
        this.maxTime = Math.max(...this.otherDurations);
        this.costProb = this.maxCost / this.otherCosts.reduce((a, b) => a + b);
        this.timeProb = this.maxTime / this.otherDurations.reduce((a, b) => a + b);
    }
}

class Trunks {
    constructor() {
        this.allTrunksEver = {};
        this.allCurrentTrunksById = {};
        this.trunks = [];
        this.branches = [];
        this.branchesTrunksMap = {};
        this.formerBranchesTrunksMap = {};
        this.formerBranches = [];
        this.formerTrunksArr = [];
        this.trunksBefore2 = [];
        this.brancesOf7738 = [];
    }

    csvJSONTOTrunks(csv, iteration){
        const lines = csv.split("\n");
        const headers = lines[0].split(",");
        for(let i = 1; i < lines.length; i++) {
            const currentLine = lines[i].split(",");
            if (currentLine.length <= 1) { // end of file
                for (const trunkId of this.formerTrunksArr){
                    if(this.trunks.indexOf(trunkId) < 0){
                        this.allTrunksEver[this.allCurrentTrunksById[trunkId].key] = new TrunkSummary(this.allCurrentTrunksById[trunkId]);
                        delete this.allCurrentTrunksById[trunkId];
                    }
                }
                this.trunksBefore2 = [...this.formerTrunksArr];
                this.formerTrunksArr = [...this.trunks];
                this.trunks = [];
                this.formerBranches = [...this.branches];
                this.branches = [];
                this.formerBranchesTrunksMap = {...this.branchesTrunksMap};
                this.branchesTrunksMap = {};
                return;
            }
            this.analyzeOneLine(iteration, headers, currentLine);
        }
    }

    analyzeOneLine(iteration, headers, currentLine){
        const linkObj = {};
        for (let j = 0; j < headers.length; j++) {
            linkObj[headers[j]] = currentLine[j];
        }

        if (linkObj.linkId === linkObj.trunk) { // this is a trunk
            const trunkId = Number.parseInt(linkObj.trunk);
            if (this.trunks.indexOf(trunkId) < 0) { // first appearance on this iter
                if (!this.allCurrentTrunksById[trunkId]) { // first appearance ever
                    const thisTrunk = new Trunk(trunkId, Number.parseInt(linkObj.loadedFor), iteration);
                    thisTrunk.generationCalculate(this.formerBranches, this.trunksBefore2, this.allCurrentTrunksById, this.formerBranchesTrunksMap);
                    this.allCurrentTrunksById[trunkId] = thisTrunk;
                }
                this.trunks.push(trunkId);
                this.allCurrentTrunksById[trunkId].wtCost = Number.parseInt(linkObj.wTCost);
                this.allCurrentTrunksById[trunkId].time = iteration - this.allCurrentTrunksById[trunkId].startIteration + 1 ;

            }
        } else { // this link is not a trunk
            const linkId = Number.parseInt(linkObj.linkId);
            this.branches.push(linkId);
            this.branchesTrunksMap[`B${linkId}`] = Number.parseInt(linkObj.trunk);
        }
    }

    async saveTrunks(){
        const TrunksModel = mongoose.model('pb-trunks', trunkSchema);
        TrunksModel.collection.insertMany(Object.values(this.allTrunksEver))
            .then(function(docs) {
                Promise.resolve();
            })
            .catch(function(err) {
                Promise.reject();
            });
    }
}
