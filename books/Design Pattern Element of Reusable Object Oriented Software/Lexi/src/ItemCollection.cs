using System.Collections;

namespace Lexi
{
    internal class ItemCollection<T> : IEnumerable<T> 
    {
        public ItemCollection(params T[] children)
        {
            foreach (var child in children)
            {
                Add(child);
            }
        }
        private readonly List<T> items = [];
        public void Add(T t) => items.Add(t);
        public void Insert(T t, int index) => items.Insert(index, t);
        public void Remove(T t) => items.Remove(t);
        public IEnumerator<T> GetEnumerator()
        {
            for (int i = 0; i < Count; i++)
            {
                yield return items[i];
            }
        }
        public IEnumerable<T> FrontToBack  => this;
        public IEnumerable<T> BackToFront
        {
            get 
            {
                for (int i = Count - 1; i >= 0; i--)
                {
                    yield return items[i];
                }
            }
        }
        public IEnumerable<T> FromToStep(int from, int to, int step)
        {
            for (int i = from; i <= to; i += step)
            {
                yield return items[i];
            }
        }

        IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
        
        // Gets number of items
        public int Count { get => items.Count; }
        // System.Collections.IEnumerable member implementation
    }
}
