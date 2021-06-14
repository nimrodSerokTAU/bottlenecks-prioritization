
exports.stringToPoint = function(locationString){
    const point = locationString.split("_");
    return({type: "point", coordinates:[Number.parseFloat(point[0]), Number.parseFloat(point[1])]});
};

exports.calcDistance = function(orig, dest) {
    if (!orig || !dest) {
        return -1;
    }
    const lon1 = (orig[0] * Math.PI) / 180;
    const lon2 = (dest[0] * Math.PI) / 180;
    const lat1 = (orig[1] * Math.PI) / 180;
    const lat2 = (dest[1] * Math.PI) / 180;

    const a = 6378137;
    const es = 0.00669437999014132;
    const dlon = lon2 - lon1;
    const dlat = lat2 - lat1;
    const avgLat = (lat2 + lat1) / 2;
    const rlon = (a * Math.cos(avgLat)) / Math.sqrt(1 - es * Math.pow(Math.sin(avgLat), 2));
    const rlat = (a * (1 - es)) / Math.pow(1 - es * Math.pow(Math.sin(avgLat), 2), 1.5);
    const x = dlon * rlon;
    const y = dlat * rlat;
    return Number.parseInt(Math.max(Math.sqrt(x * x + y * y), 1).toFixed(0), 10);
    // according to https://groups.google.com/forum/#!msg/sci.geo.satellite-nav/KRX55VzvzKo/lWOYir-g_AsJ
};




