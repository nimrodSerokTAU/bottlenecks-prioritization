const mongoose = require('mongoose');

exports.TrunkSchema = new mongoose.Schema(
    {
        _id: { type: mongoose.SchemaTypes.ObjectId },
        generation: { type: Number},
        costProb: { type: Number},
        timeProb: { type: Number},
        maxCost: { type: Number},
        maxTime: { type: Number},
        createdAt: { type: mongoose.SchemaTypes.Date },
    },
    {
        collection: 'pb-trunks',
    },
).index({
    center: '2dsphere',
});
