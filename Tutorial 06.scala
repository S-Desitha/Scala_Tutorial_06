object CaesarCipher {

  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  def shiftChar(c: Char, shift: Int): Char = {
    val index = alphabet.indexOf(c.toUpper)
    if (index == -1) c 
    else {
      val newIndex = (index + shift) % 26
      if (c.isUpper) alphabet.charAt(newIndex)
      else alphabet.charAt(newIndex).toLower
    }
  }

  def encrypt(text: String, shift: Int): String = {
    val encryptedText = text.map(c => {
      if (c.isLetter) shiftChar(c, shift)
      else c
    })
    encryptedText
  }

  def decrypt(text: String, shift: Int): String = {
    encrypt(text, -shift) 
  }

  def cipher(text: String, shift: Int, isEncrypt: Boolean): String = {
    if (isEncrypt) {
      encrypt(text, shift)
    } else {
      decrypt(text, shift)
    }
  }

  def main(args: Array[String]): Unit = {
    val text = "HeLlo woRld"
    val shift = 3
    val encryptedText = cipher(text, shift, isEncrypt = true)
    val decryptedText = cipher(encryptedText, shift, isEncrypt = false)
    println(s"Original text: $text")
    println(s"Encrypted text: $encryptedText")
    println(s"Decrypted text: $decryptedText")
  }
}
