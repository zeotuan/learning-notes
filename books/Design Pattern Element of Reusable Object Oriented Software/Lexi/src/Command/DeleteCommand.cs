using Lexi.Models;

namespace Lexi.Command
{
    internal class DeleteCommand(ICompositeGlyph document, int fromIndex, int toIndex) : ICommand
    {
        private ICompositeGlyph document = document;
        private int fromIndex = fromIndex;
        private int toIndex = toIndex;
        private IGlyph[] deletedGlyph = new IGlyph[toIndex - fromIndex + 1];

        public bool Execute()
        {
            try
            {
                for (int i = fromIndex; i <= toIndex; i++)
                {
                    var toBeRemoved = document.Children[i];
                    deletedGlyph[i-fromIndex] = toBeRemoved;
                    document.Remove(toBeRemoved);
                }
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
                for (int i = fromIndex; i <= toIndex; i++)
                {
                    var toBeRestored = deletedGlyph[i- fromIndex];
                    this.document.Insert(toBeRestored, i);
                }
            }
            catch
            {
                return false;
            }

            return true;
        }
    }
}
