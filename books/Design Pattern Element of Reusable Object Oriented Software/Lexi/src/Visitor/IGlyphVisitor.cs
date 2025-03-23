using Lexi.Models;

namespace Lexi.Visitor
{
    internal interface IGlyphVisitor
    {
        void VisitCharacter(Character ch)
        {
        }

        void VisitRow(Row row)
        {
        }

        void VisitImage(ImageGlyph image)
        {
        }

    }
}
