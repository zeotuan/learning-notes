namespace Lexi.Models
{
    internal interface ICompositeGlyph : IGlyph
    {
        void Insert(IGlyph glyph, int index);
        void Remove(IGlyph  glyph);
        IGlyph Parent { get; }

        ItemCollection<IGlyph> Children { get; }
    }
}
