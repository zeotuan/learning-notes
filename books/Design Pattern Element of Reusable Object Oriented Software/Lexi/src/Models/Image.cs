using System.Drawing;
using Lexi.Visitor;

namespace Lexi.Models
{
    internal class ImageGlyph(string imagePath) : IGlyph
    {
        private readonly Lazy<Image> bufferedImage = new Lazy<Image>(() => Image.FromFile(imagePath));

        public void Draw(Graphics window, PointF point)
        {
            window.DrawImage(bufferedImage.Value, point);
        }

        public RectangleF Bounds(PointF point)
        {
            var a = GraphicsUnit.Document;
            return bufferedImage.Value.GetBounds(ref a);
        }

        public bool Intersects(PointF current, PointF other)
        {
            return Bounds(current).IntersectsWith(new RectangleF(other, SizeF.Empty));
        }

        public bool Accept(IGlyphVisitor visitor)
        {
            try
            {
                visitor.VisitImage(this);
            }
            catch
            {
                return false;
            }

            return true;
        }
    }
}
