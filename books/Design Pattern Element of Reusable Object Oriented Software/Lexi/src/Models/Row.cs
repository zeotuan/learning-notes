using System.Drawing;
using Lexi.Visitor;

namespace Lexi.Models
{
    internal class Row : ICompositeGlyph
    {
        public ItemCollection<IGlyph> Children { get; }

        public IGlyph Parent { get; }

        public Row(IGlyph parent, params IGlyph[] children)
        {
            Parent = parent;
            Children = new ItemCollection<IGlyph>(children);
        }

        public void Insert(IGlyph glyph, int index)
        {
            Children.Insert(glyph, index);
        }

        public void Remove(IGlyph glyph)
        {
            Children.Remove(glyph);
        }

        public void Draw(Graphics window, PointF point)
        {
            var currPos = point;
            foreach (var child in Children)
            {
                child.Draw(window, point);
                currPos = currPos with { X = currPos.X + child.Bounds(currPos).Width + 2 };
            }
        }

        public RectangleF Bounds(PointF point)
        {
            float height = 0;
            float width = 0;
            foreach (var child in Children)
            {
                var bound = child.Bounds(point);
                height = height < bound.Height ? bound.Height : height;
                width += bound.Width;
            }

            return new RectangleF(point, new SizeF(width, height));
        }

        public bool Intersects(PointF current, PointF other)
        {
            return Bounds(current).IntersectsWith(new RectangleF(other, SizeF.Empty));
        }
        public bool Accept(IGlyphVisitor visitor)
        {
            try
            {
                visitor.VisitRow(this);
            }
            catch
            {
                return false;
            }

            return true;
        }
    }
}
