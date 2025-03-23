using System.Drawing;
using Lexi.Visitor;

namespace Lexi.Models
{
    internal class CompositionGlyph(ICompositeGlyph glyph, ICompositor compositor) : ICompositeGlyph
    {
        private ICompositeGlyph glyph = glyph;
        private readonly ICompositor compositor = compositor;

        public IGlyph Parent => glyph.Parent;
        public ItemCollection<IGlyph> Children  => glyph.Children;

        public void Insert(IGlyph glyph, int index)
        {
            this.glyph.Insert(glyph, index);
            compositor.Compose(this.glyph);
        }

        public void Remove(IGlyph glyph)
        {
            this.glyph.Remove(glyph);
            compositor.Compose(this.glyph);
        }

        public void Draw(Graphics window, PointF point)
        {
            this.glyph.Draw(window, point);
        }

        public RectangleF Bounds(PointF point)
        {
            return this.glyph.Bounds(point);
        }

        public bool Intersects(PointF current, PointF other)
        {
            return this.glyph.Intersects(current, other);
        }

        public bool Accept(IGlyphVisitor visitor)
        {
            try
            {
                this.glyph.Accept(visitor);
            }
            catch
            {
                return false;
            }

            return true;
        }
    }
}
