(function() {

var HxOverrides = function() { };
HxOverrides.cca = function(s,index) {
	var x = s.charCodeAt(index);
	if(x != x) return undefined;
	return x;
};
HxOverrides.substr = function(s,pos,len) {
	if(pos != null && pos != 0 && len != null && len < 0) return "";
	if(len == null) len = s.length;
	if(pos < 0) {
		pos = s.length + pos;
		if(pos < 0) pos = 0;
	} else if(len < 0) len = s.length + len - pos;
	return s.substr(pos,len);
};
var Std = function() { };
Std.parseInt = function(x) {
	var v = parseInt(x,10);
	if(v == 0 && (HxOverrides.cca(x,1) == 120 || HxOverrides.cca(x,1) == 88)) v = parseInt(x);
	if(isNaN(v)) return null;
	return v;
};
var StringTools = function() { };
StringTools.hex = function(n,digits) {
	var s = "";
	var hexChars = "0123456789ABCDEF";
	do {
		s = hexChars.charAt(n & 15) + s;
		n >>>= 4;
	} while(n > 0);
	if(digits != null) while(s.length < digits) s = "0" + s;
	return s;
};
var husl_ColorPicker = function() { };
husl_ColorPicker.getPickerGeometry = function(lightness) {
	var lines = husl_Husl.getBounds(lightness);
	var numLines = lines.length;
	var outerCircleRadius = 0.0;
	var closestindex2 = null;
	var closestLineDistance = null;
	var _g = 0;
	while(_g < numLines) {
		var i = _g++;
		var d = husl_Geometry.distanceLineFromOrigin(lines[i]);
		if(closestLineDistance == null || d < closestLineDistance) {
			closestLineDistance = d;
			closestindex2 = i;
		}
	}
	var closestLine = lines[closestindex2];
	var perpendicularLine = { slope : 0 - 1 / closestLine.slope, intercept : 0.0};
	var intersectionPoint = husl_Geometry.intersectLineLine(closestLine,perpendicularLine);
	var startingAngle = husl_Geometry.angleFromOrigin(intersectionPoint);
	var intersections = [];
	var intersectionPoint1;
	var intersectionPointAngle;
	var relativeAngle;
	var _g1 = 0;
	var _g2 = numLines - 1;
	while(_g1 < _g2) {
		var i1 = _g1++;
		var _g21 = i1 + 1;
		while(_g21 < numLines) {
			var i2 = _g21++;
			intersectionPoint1 = husl_Geometry.intersectLineLine(lines[i1],lines[i2]);
			intersectionPointAngle = husl_Geometry.angleFromOrigin(intersectionPoint1);
			relativeAngle = intersectionPointAngle - startingAngle;
			intersections.push({ line1 : i1, line2 : i2, intersectionPoint : intersectionPoint1, intersectionPointAngle : intersectionPointAngle, relativeAngle : husl_Geometry.normalizeAngle(intersectionPointAngle - startingAngle)});
		}
	}
	intersections.sort(function(a,b) {
		if(a.relativeAngle > b.relativeAngle) return 1; else return -1;
	});
	var orderedLines = [];
	var orderedVertices = [];
	var orderedAngles = [];
	var currentindex2;
	var nextindex2;
	var currentIntersection;
	var intersectionPointDistance;
	var currentindex21 = closestindex2;
	var _g11 = 0;
	var _g3 = intersections.length;
	while(_g11 < _g3) {
		var j = _g11++;
		currentIntersection = intersections[j];
		nextindex2 = null;
		if(currentIntersection.line1 == currentindex21) nextindex2 = currentIntersection.line2; else if(currentIntersection.line2 == currentindex21) nextindex2 = currentIntersection.line1;
		if(nextindex2 != null) {
			currentindex21 = nextindex2;
			orderedLines.push(lines[nextindex2]);
			orderedVertices.push(currentIntersection.intersectionPoint);
			orderedAngles.push(currentIntersection.intersectionPointAngle);
			intersectionPointDistance = husl_Geometry.distanceFromOrigin(currentIntersection.intersectionPoint);
			if(intersectionPointDistance > outerCircleRadius) outerCircleRadius = intersectionPointDistance;
		}
	}
	return { lines : orderedLines, vertices : orderedVertices, angles : orderedAngles, outerCircleRadius : outerCircleRadius, innerCircleRadius : closestLineDistance};
};
husl_ColorPicker.closestPoint = function(geometry,point) {
	var angle = husl_Geometry.angleFromOrigin(point);
	var numVertices = geometry.vertices.length;
	var relativeAngle;
	var smallestRelativeAngle = Math.PI * 2;
	var index1 = 0;
	var _g = 0;
	while(_g < numVertices) {
		var i = _g++;
		relativeAngle = husl_Geometry.normalizeAngle(geometry.angles[i] - angle);
		if(relativeAngle < smallestRelativeAngle) {
			smallestRelativeAngle = relativeAngle;
			index1 = i;
		}
	}
	var index2 = (index1 - 1 + numVertices) % numVertices;
	var closestLine = geometry.lines[index2];
	if(husl_Geometry.distanceFromOrigin(point) < husl_Geometry.lengthOfRayUntilIntersect(angle,closestLine)) return point;
	var perpendicularLine = husl_Geometry.perpendicularThroughPoint(closestLine,point);
	var intersectionPoint = husl_Geometry.intersectLineLine(closestLine,perpendicularLine);
	var bound1 = geometry.vertices[index1];
	var bound2 = geometry.vertices[index2];
	var upperBound;
	var lowerBound;
	if(bound1.x > bound2.x) {
		upperBound = bound1;
		lowerBound = bound2;
	} else {
		upperBound = bound2;
		lowerBound = bound1;
	}
	var borderPoint;
	if(intersectionPoint.x > upperBound.x) borderPoint = upperBound; else if(intersectionPoint.x < lowerBound.x) borderPoint = lowerBound; else borderPoint = intersectionPoint;
	return borderPoint;
};
var husl_Geometry = function() { };
husl_Geometry.intersectLineLine = function(a,b) {
	var x = (a.intercept - b.intercept) / (b.slope - a.slope);
	var y = a.slope * x + a.intercept;
	return { x : x, y : y};
};
husl_Geometry.distanceFromOrigin = function(point) {
	return Math.sqrt(Math.pow(point.x,2) + Math.pow(point.y,2));
};
husl_Geometry.distanceLineFromOrigin = function(line) {
	return Math.abs(line.intercept) / Math.sqrt(Math.pow(line.slope,2) + 1);
};
husl_Geometry.perpendicularThroughPoint = function(line,point) {
	var slope = -1 / line.slope;
	var intercept = point.y - slope * point.x;
	return { slope : slope, intercept : intercept};
};
husl_Geometry.angleFromOrigin = function(point) {
	return Math.atan2(point.y,point.x);
};
husl_Geometry.normalizeAngle = function(angle) {
	var m = 2 * Math.PI;
	return (angle % m + m) % m;
};
husl_Geometry.lengthOfRayUntilIntersect = function(theta,line) {
	return line.intercept / (Math.sin(theta) - line.slope * Math.cos(theta));
};
var husl_Husl = function() { };

husl_Husl.getBounds = function(L) {
	var result = [];
	var sub1 = Math.pow(L + 16,3) / 1560896;
	var sub2;
	if(sub1 > husl_Husl.epsilon) sub2 = sub1; else sub2 = L / husl_Husl.kappa;
	var _g = 0;

    console.log('> sub1', sub1);
    console.log('> sub2', sub2);

	while(_g < 3) {
		var c = _g++;
		var m1 = husl_Husl.m[c][0];
		var m2 = husl_Husl.m[c][1];
		var m3 = husl_Husl.m[c][2];
		var _g1 = 0;
		while(_g1 < 2) {
			var t = _g1++;
			var top1 = (284517 * m1 - 94839 * m3) * sub2;
			var top2 = (838422 * m3 + 769860 * m2 + 731718 * m1) * L * sub2 - 769860 * t * L;
			  // var top2 = (838422 * m3 + 769860 * m2 + 731718 * m1);
			var bottom = (632260 * m3 - 126452 * m2) * sub2 + 126452 * t;
        console.log('> ----');
        console.log('> t', t);
        console.log('> m1', m1);
        console.log('> m2', m2);
        console.log('> m3', m3);
        console.log('> top1', top1);
        console.log('> top2', top2);
        console.log('> bottom', bottom);
			result.push({ slope : top1 / bottom, intercept : top2 / bottom});
		}
	}
	return result;
};

husl_Husl.maxSafeChromaForL = function(L) {
	var bounds = husl_Husl.getBounds(L);
	var min = 1.7976931348623157e+308;
	var _g = 0;
	while(_g < 2) {
		var i = _g++;
		var length = husl_Geometry.distanceLineFromOrigin(bounds[i]);
		min = Math.min(min,length);
	}
	return min;
};
husl_Husl.maxChromaForLH = function(L,H) {
	var hrad = H / 360 * Math.PI * 2;
	var bounds = husl_Husl.getBounds(L);
	var min = 1.7976931348623157e+308;
	var _g = 0;
	while(_g < bounds.length) {
		var bound = bounds[_g];
		++_g;
		var length = husl_Geometry.lengthOfRayUntilIntersect(hrad,bound);
		if(length >= 0) min = Math.min(min,length);
	}
	return min;
};
husl_Husl.dotProduct = function(a,b) {
	var sum = 0;
	var _g1 = 0;
	var _g = a.length;
	while(_g1 < _g) {
		var i = _g1++;
		sum += a[i] * b[i];
	}
	return sum;
};
husl_Husl.fromLinear = function(c) {
	if(c <= 0.0031308) return 12.92 * c; else return 1.055 * Math.pow(c,0.416666666666666685) - 0.055;
};
husl_Husl.toLinear = function(c) {
	if(c > 0.04045) return Math.pow((c + 0.055) / 1.055,2.4); else return c / 12.92;
};
husl_Husl.xyzToRgb = function(tuple) {
	return [husl_Husl.fromLinear(husl_Husl.dotProduct(husl_Husl.m[0],tuple)),husl_Husl.fromLinear(husl_Husl.dotProduct(husl_Husl.m[1],tuple)),husl_Husl.fromLinear(husl_Husl.dotProduct(husl_Husl.m[2],tuple))];
};
husl_Husl.rgbToXyz = function(tuple) {
	var rgbl = [husl_Husl.toLinear(tuple[0]),husl_Husl.toLinear(tuple[1]),husl_Husl.toLinear(tuple[2])];
	return [husl_Husl.dotProduct(husl_Husl.minv[0],rgbl),husl_Husl.dotProduct(husl_Husl.minv[1],rgbl),husl_Husl.dotProduct(husl_Husl.minv[2],rgbl)];
};
husl_Husl.yToL = function(Y) {
	if(Y <= husl_Husl.epsilon) return Y / husl_Husl.refY * husl_Husl.kappa; else return 116 * Math.pow(Y / husl_Husl.refY,0.333333333333333315) - 16;
};
husl_Husl.lToY = function(L) {
	if(L <= 8) return husl_Husl.refY * L / husl_Husl.kappa; else return husl_Husl.refY * Math.pow((L + 16) / 116,3);
};
husl_Husl.xyzToLuv = function(tuple) {
	var X = tuple[0];
	var Y = tuple[1];
	var Z = tuple[2];
	var divider = X + 15 * Y + 3 * Z;
	var varU = 4 * X;
	var varV = 9 * Y;
	if(divider != 0) {
		varU /= divider;
		varV /= divider;
	} else {
		varU = NaN;
		varV = NaN;
	}
	var L = husl_Husl.yToL(Y);
	if(L == 0) return [0,0,0];
	var U = 13 * L * (varU - husl_Husl.refU);
	var V = 13 * L * (varV - husl_Husl.refV);
	return [L,U,V];
};
husl_Husl.luvToXyz = function(tuple) {
	var L = tuple[0];
	var U = tuple[1];
	var V = tuple[2];
	if(L == 0) return [0,0,0];
	var varU = U / (13 * L) + husl_Husl.refU;
	var varV = V / (13 * L) + husl_Husl.refV;
	var Y = husl_Husl.lToY(L);
	var X = 0 - 9 * Y * varU / ((varU - 4) * varV - varU * varV);
	var Z = (9 * Y - 15 * varV * Y - varV * X) / (3 * varV);
	return [X,Y,Z];
};
husl_Husl.luvToLch = function(tuple) {
	var L = tuple[0];
	var U = tuple[1];
	var V = tuple[2];
	var C = Math.sqrt(U * U + V * V);
	var H;
	if(C < 0.00000001) H = 0; else {
		var Hrad = Math.atan2(V,U);
		H = Hrad * 180.0 / 3.1415926535897932;
		if(H < 0) H = 360 + H;
	}
	return [L,C,H];
};
husl_Husl.lchToLuv = function(tuple) {
	var L = tuple[0];
	var C = tuple[1];
	var H = tuple[2];
	var Hrad = H / 360.0 * 2 * Math.PI;
	var U = Math.cos(Hrad) * C;
	var V = Math.sin(Hrad) * C;
	return [L,U,V];
};
husl_Husl.huslToLch = function(tuple) {
	var H = tuple[0];
	var S = tuple[1];
	var L = tuple[2];
	if(L > 99.9999999) return [100,0,H];
	if(L < 0.00000001) return [0,0,H];
	var max = husl_Husl.maxChromaForLH(L,H);
	var C = max / 100 * S;
	return [L,C,H];
};
husl_Husl.lchToHusl = function(tuple) {
	var L = tuple[0];
	var C = tuple[1];
	var H = tuple[2];
	if(L > 99.9999999) return [H,0,100];
	if(L < 0.00000001) return [H,0,0];
	var max = husl_Husl.maxChromaForLH(L,H);
	var S = C / max * 100;
	return [H,S,L];
};
husl_Husl.huslpToLch = function(tuple) {
	var H = tuple[0];
	var S = tuple[1];
	var L = tuple[2];
	if(L > 99.9999999) return [100,0,H];
	if(L < 0.00000001) return [0,0,H];
	var max = husl_Husl.maxSafeChromaForL(L);
	var C = max / 100 * S;
	return [L,C,H];
};
husl_Husl.lchToHuslp = function(tuple) {
	var L = tuple[0];
	var C = tuple[1];
	var H = tuple[2];
	if(L > 99.9999999) return [H,0,100];
	if(L < 0.00000001) return [H,0,0];
	var max = husl_Husl.maxSafeChromaForL(L);
	var S = C / max * 100;
	return [H,S,L];
};
husl_Husl.rgbToHex = function(tuple) {
	var h = "#";
	var _g1 = 0;
	var _g = tuple.length;
	while(_g1 < _g) {
		var i = _g1++;
		var chan = tuple[i];
		h += StringTools.hex(Math.round(chan * 255),2);
	}
	return h;
};
husl_Husl.hexToRgb = function(hex) {
	hex = hex.toUpperCase();
	return [Std.parseInt("0x" + HxOverrides.substr(hex,1,2)) / 255.0,Std.parseInt("0x" + HxOverrides.substr(hex,3,2)) / 255.0,Std.parseInt("0x" + HxOverrides.substr(hex,5,2)) / 255.0];
};
husl_Husl.lchToRgb = function(tuple) {
	return husl_Husl.xyzToRgb(husl_Husl.luvToXyz(husl_Husl.lchToLuv(tuple)));
};
husl_Husl.rgbToLch = function(tuple) {
	return husl_Husl.luvToLch(husl_Husl.xyzToLuv(husl_Husl.rgbToXyz(tuple)));
};
husl_Husl.huslToRgb = function(tuple) {
	return husl_Husl.lchToRgb(husl_Husl.huslToLch(tuple));
};
husl_Husl.rgbToHusl = function(tuple) {
	return husl_Husl.lchToHusl(husl_Husl.rgbToLch(tuple));
};
husl_Husl.huslpToRgb = function(tuple) {
	return husl_Husl.lchToRgb(husl_Husl.huslpToLch(tuple));
};
husl_Husl.rgbToHuslp = function(tuple) {
	return husl_Husl.lchToHuslp(husl_Husl.rgbToLch(tuple));
};
husl_Husl.huslToHex = function(tuple) {
	return husl_Husl.rgbToHex(husl_Husl.huslToRgb(tuple));
};
husl_Husl.huslpToHex = function(tuple) {
	return husl_Husl.rgbToHex(husl_Husl.huslpToRgb(tuple));
};
husl_Husl.hexToHusl = function(s) {
	return husl_Husl.rgbToHusl(husl_Husl.hexToRgb(s));
};
husl_Husl.hexToHuslp = function(s) {
	return husl_Husl.rgbToHuslp(husl_Husl.hexToRgb(s));
};
husl_Husl.m = [[3.240969941904521,-1.537383177570093,-0.498610760293],[-0.96924363628087,1.87596750150772,0.041555057407175],[0.055630079696993,-0.20397695888897,1.056971514242878]];
husl_Husl.minv = [[0.41239079926595,0.35758433938387,0.18048078840183],[0.21263900587151,0.71516867876775,0.072192315360733],[0.019330818715591,0.11919477979462,0.95053215224966]];
husl_Husl.refY = 1.0;
husl_Husl.refU = 0.19783000664283;
husl_Husl.refV = 0.46831999493879;
husl_Husl.kappa = 903.2962962;
husl_Husl.epsilon = 0.0088564516;
var exportObject = {
    'Husl': husl_Husl,
    'Geometry': husl_Geometry,
    'ColorPicker': husl_ColorPicker
};// TODO: Deprecated. Remove with the next major version
// Export to jQuery
if (typeof jQuery !== 'undefined') {
    jQuery['husl'] = exportObject;
}

// CommonJS module system (including Node)
if (typeof module !== 'undefined') {
    module['exports'] = exportObject;
}

// AMD module system
if (typeof define !== 'undefined') {
    define(exportObject);
}

// Export to browser
if (typeof window !== 'undefined') {
    window['HUSL'] = exportObject;
}})();

