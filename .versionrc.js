const re = /Version: (\d\.\d\.\d)/;

function readVersion(contents) {
	const matches = contents.match(re);
	return matches[1];
}

function writeVersion(contents, version) {
	return contents.replace(re, `Version: ${version}`);
}

const updater = { readVersion, writeVersion };

const trackers = [
	{
		filename: "modaled.el",
		updater
	}
];

module.exports = {
	// read version
	packageFiles: trackers,
	// write version
	bumpFiles: trackers
};