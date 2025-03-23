using System.Drawing;
using Lexi.Visitor;

namespace Lexi.Models
{
    internal class Character(char character, Font font, Brush? brush) : IGlyph
    {

        public char Char { get; set; } = character;
        public Font Font { get; set; } = font;

        public Brush Brush { get; set; } = brush ?? new SolidBrush(Color.Black);

        public void Draw(Graphics window, PointF point)
        {
            window.DrawString(Char.ToString(), font, Brush, point);
        }

        public RectangleF Bounds(PointF point)
        {
            using Graphics temp = Graphics.FromImage(new Bitmap(1, 1));
            SizeF size = temp.MeasureString(Char.ToString(), Font);
            return new RectangleF(point, size);
        }

        public bool Intersects(PointF current, PointF other)
        {
            return Bounds(current).IntersectsWith(Bounds(other));
        }

        public bool Accept(IGlyphVisitor visitor)
        {
            try
            {
                visitor.VisitCharacter(this);
            }
            catch
            {
                return false;
            }

            return true;
        }

        public int getCharacterCode() => Char;
    }
}
