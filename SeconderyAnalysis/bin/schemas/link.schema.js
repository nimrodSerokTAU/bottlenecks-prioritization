const mongoose = require('mongoose');

exports.LinkSchema = new mongoose.Schema(
    {
        _id: { type: mongoose.SchemaTypes.ObjectId },
        linkId: { type: Number },
        origin: {type: mongoose.SchemaTypes.Mixed},
        destination: {type: mongoose.SchemaTypes.Mixed},
        bottleNecks: {},
        maxDist: { type: Number },
        medianDist: { type: Number },
        maxSimDist: { type: Number },
        medianSimDist: { type: Number },
        sumCostOfTrees: { type: String },
        sumOfOwnCost: { type: Number },
        upTo90Count: { type: Number },
        upTo100Count: { type: Number },
        createdAt: { type: mongoose.SchemaTypes.Date },
    },
    {
        collection: 'pb-links',
    },
).index({
    location: '2dsphere',
});
