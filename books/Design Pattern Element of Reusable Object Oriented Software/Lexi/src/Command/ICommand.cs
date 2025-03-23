namespace Lexi.Command
{
    internal interface ICommand
    {
        bool Execute();
        bool UnExecute();
    }
}
