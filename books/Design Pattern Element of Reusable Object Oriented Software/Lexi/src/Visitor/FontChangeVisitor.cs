using System.Drawing;
using Lexi.Models;

namespace Lexi.Visitor
{
    internal class FontChangeVisitor(Font newFont) : IGlyphVisitor
    {
        private Font newFont = newFont;
        public List<Font>? PreviousFonts = new();

        public void VisitCharacter(Character ch)
        {
            PreviousFonts.Add(ch.Font);
            ch.Font = newFont;
        }

        public void VisitRow(Row row)
        {
            foreach (var glyph in row.Children.FrontToBack)
            {
                glyph.Accept(this);
            }
        }
    }
}
