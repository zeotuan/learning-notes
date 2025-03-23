using System.Text;
using Lexi.Models;

namespace Lexi.Visitor
{
    internal class SpellCheckVisitor(ISpellChecker spellChecker) : IGlyphVisitor
    {
        private StringBuilder currWord = new();
        private List<IGlyph> currentGlyphs = new();
        private ISpellChecker spellChecker = spellChecker;
        public List<string> MissSpelling { get; } = new();



        public void VisitCharacter(Character ch)
        {
            var c = ch.Char;
            if (char.IsAsciiLetter(c) || char.IsAsciiDigit(c))
            {
                currWord.Append(c);
                currentGlyphs.Add(ch);
                return;
            }
            SpellCheck();
        }
        public void VisitRow(Row row)
        {
            foreach (var glyph in row.Children.FrontToBack)
            {
                glyph.Accept(this);
            }
        }

        private void SpellCheck()
        {
            string word = currWord.ToString();
            if (!string.IsNullOrWhiteSpace(word))
            {
                if (!spellChecker.CheckSpell(word))
                {
                    MissSpelling.Add(word);
                }
            }

            currentGlyphs.Clear();
            currWord.Clear();
        }
    }
}
