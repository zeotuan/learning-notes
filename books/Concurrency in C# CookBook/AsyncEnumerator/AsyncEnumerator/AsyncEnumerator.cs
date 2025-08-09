namespace AsyncEnumerator
{
    // Delegate for the Morph method
    public delegate Int32 Morpher<TResult, TArgument>(Int32 input, TArgument argument, out TResult result);

    public class AsyncEnumerator {
        public AsyncEnumerator()
        {
        }

        // Refers to the iterator member's code
       private IEnumerator<Int32> m_enumerator;

       // Collection of completed async operations (the inbox)
       private List<IAsyncResult> m_inbox = new List<IAsyncResult>();

       // Structure with Wait & Inbox counters
       private WaitAndInboxCounts m_waitAndInboxCounts;

        // Methods called by code outside of the iterator
        public void Execute(IEnumerator<Int32> enumerator);

        // Methods called by code inside the iterator
        public AsyncCallback End();
        public IAsyncResult DequeueAsyncResult();

        private void ResumeIterator() {
           Boolean continueIterating;

           // While there are more operations to perform...
           while (continueIterating = m_enumerator.MoveNext()) {

              // Get the value returned from the enumerator
              UInt16 numberOpsToWaitFor = checked((UInt16) m_enumerator.Current);

              // If inbox has fewer items than requested, keep iterator suspended
              if (!m_waitAndInboxCounts.AtomicSetWait(numberOpsToWaitFor)) break;

              // Inbox has enough items, loop to resume the iterator
           }

           // The iterator is suspended, just return
           if (continueIterating) return;

           // The iterator has exited, execute the iterator's finally code
           m_enumerator.Dispose();
        }

        private void EnqueueAsyncResult(IAsyncResult result) {
           // Add this item to the inbox
           lock (m_inbox) { m_inbox.Add(result); }

           // Add 1 to inbox count. If inbox has enough items 
           // in it; this thread calls ResumeIterator
           if (m_waitAndInboxCounts.AtomicIncrementInbox()) 
              ResumeIterator();
        }

        private struct WaitAndInboxCounts {
           private const UInt16 c_MaxWait = 0xFFFF;
           // Wait=High 16 bits, Inbox=low-16 bits
           private Int32 m_waitAndInboxCounts;    

           private UInt16 Wait {
              get { return (UInt16) (m_waitAndInboxCounts >> 16); }
              set { m_waitAndInboxCounts = (Int32) ((value << 16) | Inbox); }
           }
           private UInt16 Inbox {
              get { return (UInt16) m_waitAndInboxCounts; }
              set { m_waitAndInboxCounts = 
                 (Int32)((m_waitAndInboxCounts & 
                    0xFFFF0000)|value); }
           }

           private WaitAndInboxCounts(Int32 waic) { m_waitAndInboxCounts = waic; }
           private Int32 ToInt32() { return m_waitAndInboxCounts; }

           internal void Initialize() { Wait = c_MaxWait; }

           internal Boolean AtomicSetWait(UInt16 numberOpsToWaitFor) {
              return Morph<Boolean, UInt16>(
                 ref m_waitAndInboxCounts, 
                 numberOpsToWaitFor, SetWait);
           }

           private static Int32 SetWait(Int32 i, UInt16 numberOpsToWaitFor, 
              out Boolean shouldMoveNext) {
              WaitAndInboxCounts waic = new WaitAndInboxCounts(i);
              // Set the number of items to wait for
              waic.Wait = numberOpsToWaitFor;  
              shouldMoveNext = (waic.Inbox >= waic.Wait);

              // Does the inbox contain enough items to MoveNext?
              if (shouldMoveNext) {         
                 // Subtract the number of items from the inbox 
                 waic.Inbox -= waic.Wait;   
                 // The next wait is indefinite 
                 waic.Wait = c_MaxWait;     
              }
              return waic.ToInt32();
           }

           internal Boolean AtomicIncrementInbox() {
              return Morph<Boolean, Object>(
                 ref m_waitAndInboxCounts, 
                 null, IncrementInbox);
           }

           private static Int32 IncrementInbox(Int32 i, Object argument, 
              out Boolean shouldMoveNext) {
              WaitAndInboxCounts waic = new WaitAndInboxCounts(i);
              // Add 1 to the inbox count
              waic.Inbox++;                 
              shouldMoveNext = (waic.Inbox == waic.Wait);

              // Does the inbox contain enough items to MoveNext?
              if (shouldMoveNext) {         
                 // Subtract the number of items from the inbox 
                 waic.Inbox -= waic.Wait;   
                 // The next wait is indefinite 
                 waic.Wait = c_MaxWait;     
              }
              return waic.ToInt32();
           }

            public static TResult Morph<TResult, TArgument>(ref Int32 target, TArgument argument, Morpher<TResult, TArgument> morpher) {
               TResult morphResult;
               Int32 i, j = target;
               do {
                  i = j;
                  j = Interlocked.CompareExchange(ref target, morpher(i, argument, out morphResult), i);
               } while (i != j);
               return morphResult;
            }
        }
    }
}
