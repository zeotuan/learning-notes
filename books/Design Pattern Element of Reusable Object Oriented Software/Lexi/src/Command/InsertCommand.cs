using Lexi.Models;

namespace Lexi.Command
{
    internal class InsertCommand(ICompositeGlyph document, IGlyph newGlyph, int index) : ICommand
    {
        private ICompositeGlyph document = document;
        private IGlyph newGlyph = newGlyph;
        private int index = index;

        public bool Execute()
        {
            try
            {
                this.document.Insert(newGlyph, index);
            }
            catch
            {
                return false;
            }

            return true;
        }
        
        public bool UnExecute()
        {
            //TODO: check state to see if it's valid to undo
            try
            {
                this.document.Remove(newGlyph);
            }
            catch
            {
                return false;
            }

            return true;
        } 
    }
}
