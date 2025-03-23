using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Lexi.Models;
using Lexi.Visitor;

namespace Lexi.Command
{
    internal class ChangeFontCommand(ICompositeGlyph glyph, Font newFont) : ICommand
    {
        private ICompositeGlyph glyph = glyph;
        private FontChangeVisitor visitor = new(newFont);

        public bool Execute()
        {
            try
            {
                glyph.Accept(visitor);
            }
            catch (Exception e)
            {
                return false;
            }

            return true;
        }

        public bool UnExecute()
        {
            var previousFonts = visitor.PreviousFonts;
            if (glyph.Children.Count != previousFonts.Count)
            {
                // Debug log cannot reach this as content must be the same
                return false;
            }

            foreach (var glyphChild in glyph.Children)
            {
                if (glyphChild is Character c)
                {
                    var previous = previousFonts.Last();
                    c.Font = previous;
                    previousFonts.Remove(previous);
                }
            }

            return true;
        }
    }
}
