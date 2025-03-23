namespace Lexi.Command
{
    internal class CommandManager
    {
        private static readonly Lazy<CommandManager> instance = new(() => new CommandManager());

        private readonly Stack<ICommand> undoStack = new();
        private readonly Stack<ICommand> redoStack = new();

        private CommandManager() 
        {
        }  // Private constructor to enforce singleton

        public static CommandManager Instance => instance.Value;

        public void ExecuteCommand(ICommand command)
        {
            command.Execute();
            undoStack.Push(command);
            redoStack.Clear();  // Clear redo history after a new command
        }

        public void Undo()
        {
            if (undoStack.Count > 0)
            {
                ICommand command = undoStack.Pop();
                command.UnExecute();
                redoStack.Push(command);
            }
        }

        public void Redo()
        {
            if (redoStack.Count > 0)
            {
                ICommand command = redoStack.Pop();
                command.Execute();
                undoStack.Push(command);
            }
        }
    }
}
