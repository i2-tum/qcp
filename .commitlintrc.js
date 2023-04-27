const Configuration = {
  /*
   * Resolve and load @commitlint/config-conventional from node_modules.
   * Referenced packages must be installed
   */
  extends: ['@commitlint/config-conventional'],
  /*
   * Any rules defined here will override rules from @commitlint/config-conventional
   */
  rules: {
    'body-leading-blank': [ 2, 'always' ],
    'body-empty': [ 0, 'never' ],
    'body-max-length': [ 0, 'always', 150 ],
    'body-max-line-length': [ 2, 'always', 72 ],
    'footer-leading-blank': [ 2, 'always' ],
    'footer-max-line-length': [ 2, 'always', 72 ],
    'header-case': [ 2, 'always', 'sentence-case' ],
    'header-full-stop': [ 2, 'never', '.' ],
    'header-max-length': [ 2, 'always', 50 ],
    'header-min-length': [ 2, 'always', 4 ],
    'scope-empty': [ 2, 'always' ],
    'subject-empty': [ 2, 'always' ],
    'type-empty': [ 2, 'always' ]
  },
  /*
   * Functions that return true if commitlint should ignore the given message.
   */
  ignores: [(commit) => (/^Bump (.*) from (.*) to (.*)/).test(commit)],
};

module.exports = Configuration;
