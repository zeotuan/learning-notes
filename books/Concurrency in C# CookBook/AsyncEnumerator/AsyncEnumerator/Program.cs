static IEnumerator<Int32> ApmPatternWithIterator(
   AsyncEnumerator ae, String pathname) {
   using (FileStream fs = new FileStream(pathname, FileMode.Open,
      FileAccess.Read, FileShare.Read, 8192, FileOptions.Asynchronous)) {

      Byte[] data = new Byte[fs.Length];
      fs.BeginRead(data, 0, data.Length, ae.End(), null);
      yield return 1;

      Int32 bytesRead = fs.EndRead(ae.DequeueAsyncResult());
      ProcessData(data);
   }
}