import java.util.concurrent.locks.ReentrantLock;
class AcmeSafe implements State {
    private byte[] value;
    private byte maxval;
    private ReentrantLock loc;

    AcmeSafe(byte[] v) { value = v; maxval = 127; loc = new ReentrantLock(); }

    AcmeSafe(byte[] v, byte m) { value = v; maxval = m; loc = new ReentrantLock(); }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
    loc.lock();
	if (value[i] <= 0 || value[j] >= maxval) {
        loc.unlock();
	    return false;
	}
	value[i]--;
	value[j]++;
    loc.unlock();
	return true;
    }
}
