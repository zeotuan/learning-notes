using System.Drawing;
using Lexi.Visitor;

namespace Lexi.Models
{
    internal interface IGlyph
    {
        // appearance
        void Draw(Graphics window, PointF point);
        RectangleF Bounds(PointF point);

        // hit detection
        bool Intersects(PointF current, PointF other);

        bool Accept(IGlyphVisitor visitor);
    }
}
