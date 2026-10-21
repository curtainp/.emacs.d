// The password travels over stdin, never in command-line arguments or output.
const { randomBytes, pbkdf2Sync, createCipheriv } = require('node:crypto');
const { readFileSync } = require('node:fs');

function encryptArticle(password, content) {
  if (typeof password !== 'string' || !password.trim() || typeof content !== 'string') {
    throw new Error('A nonempty password and article content are required.');
  }
  const iterations = 600000;
  const salt = randomBytes(16);
  const iv = randomBytes(12);
  const key = pbkdf2Sync(password, salt, iterations, 32, 'sha256');
  const cipher = createCipheriv('aes-256-gcm', key, iv);
  const ciphertext = Buffer.concat([
    cipher.update(content, 'utf8'), cipher.final(), cipher.getAuthTag(),
  ]);
  key.fill(0);
  return {
    version: 1, iterations,
    salt: salt.toString('base64'),
    iv: iv.toString('base64'),
    ciphertext: ciphertext.toString('base64'),
  };
}

if (require.main === module) {
  try {
    const { password, content } = JSON.parse(readFileSync(0, 'utf8'));
    process.stdout.write(JSON.stringify(encryptArticle(password, content)));
  } catch {
    process.stderr.write('Article encryption failed; publication stopped.\n');
    process.exitCode = 1;
  }
}

module.exports = { encryptArticle };
