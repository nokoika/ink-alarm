// @ts-check

console.log("Current Working Directory:", process.cwd());
const files = process.argv.slice(2);
console.log("Files being linted:", files);

/**
 * @param {string[]} command
 * @param {string} cwd
 * @returns {Promise<void>}
 */
const exec = async (command, cwd) => {
  const progress = Bun.spawn(command, {
    cwd,
    stdin: "inherit",
    stdout: "inherit",
    stderr: "inherit",
  });
  const exitCode = await progress.exited;
  if (exitCode !== 0) {
    throw new Error(`Command failed with exit code ${exitCode}`);
  }
};

/**
 * eslint v9 からは --ext がないので、自前でフィルタリングする
 * @param {string[]} files
 * @returns {string[]}
 */
const filterLintTarget = (files) => {
  return files.filter((file) => file.match(/\.(js|jsx|ts|tsx)$/));
};

await exec(
  ["bun", "eslint", "--fix", "--max-warnings", "0", ...filterLintTarget(files)],
  "front"
);
await exec(["bun", "prettier", "--write", ...files], "front");
