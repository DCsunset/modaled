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
  types: [
    {"type": "feat", "section": "Features"},
    {"type": "fix", "section": "Bug Fixes"},
    {"type": "chore", "section": "Misc"},
    {"type": "docs", "section": "Misc"},
    {"type": "style", "section": "Misc"},
    {"type": "refactor", "section": "Misc"},
    {"type": "perf", "section": "Misc"},
    {"type": "test", "section": "Misc"},
    {"type": "ci", "section": "Misc"}
  ],
	// read version
	packageFiles: trackers,
	// write version
	bumpFiles: trackers
};
