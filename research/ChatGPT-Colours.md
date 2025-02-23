# Prompt

Can you please teach me how to include colour in IRC messages?

# Answer



Including color in IRC messages involves using special control codes that are recognized by most IRC clients. These control codes allow you to format your messages with different colors or styles like bold or underline. Here's how you can do it:

### 1. **Color Control Code**
- The control code for color is **`Ctrl + K`**, which appears as `\x03` in raw text.
- After the color code, you specify the foreground color number, optionally followed by a comma and the background color number.

### 2. **Color Codes**
Here are the standard IRC color codes:

| Code | Color          | Code | Color          |
|------|----------------|------|----------------|
| 00   | White          | 08   | Yellow         |
| 01   | Black          | 09   | Light Green    |
| 02   | Blue (Navy)    | 10   | Cyan           |
| 03   | Green          | 11   | Light Cyan     |
| 04   | Red            | 12   | Light Blue     |
| 05   | Brown (Maroon) | 13   | Pink           |
| 06   | Purple         | 14   | Grey           |
| 07   | Orange         | 15   | Light Grey     |

### 3. **Example Usage**
To include color in your messages, use the format:

```
\x03<foreground>,<background>Text
```

#### Examples:
1. **Simple colored text** (e.g., Red text):
   ```
   \x034This text is red.
   ```
   Output: *This text is red.*

2. **Text with a background color** (e.g., Green text on a Yellow background):
   ```
   \x033,08This text is green with a yellow background.
   ```
   Output: *This text is green with a yellow background.*

3. **Resetting the color**:
   To stop the color effect, you can use the reset code: **`Ctrl + O`** (`\x0F` in raw text).

   Example:
   ```
   \x034Red text\x0F Normal text
   ```
   Output: *Red text Normal text.*

### 4. **Styling Codes**
You can combine colors with other styles:

| Style         | Code   | Description                     |
|---------------|--------|---------------------------------|
| Bold          | \x02   | Makes the text bold.           |
| Underline     | \x1F   | Underlines the text.           |
| Italic        | \x1D   | Italicizes the text.           |
| Reverse Color | \x16   | Swaps foreground and background. |

#### Example with Bold and Color:
```
\x02\x034Bold and Red\x0F Normal text
```
Output: **Bold and Red** *Normal text.*

### 5. **Send Messages**
When typing in an IRC client:
- Use `Ctrl + K` to insert the color code.
- Follow with the numeric codes for foreground and background colors as needed.

If you're scripting for an IRC bot or using raw IRC commands, include the control characters as their ASCII values (`\x03`, `\x02`, etc.) directly in your message string.

Let me know if you’d like examples in a specific programming language, like Go or PHP, for sending these formatted IRC messages!
