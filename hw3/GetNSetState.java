import java.util.concurrent.atomic.AtomicIntegerArray;
class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v) { 
    int [] val = new int[v.length];
    for(int i =0; i<v.length; i++)
    {
        val[i] = v[i];
    }
    value = new AtomicIntegerArray(val);
    maxval = 127; }

    GetNSetState(byte[] v, byte m) { int [] val = new int[v.length];
    for(int i =0; i<v.length; i++)
    {
        val[i] = v[i];
    }
    value = new AtomicIntegerArray(val); maxval = m; }

    public int size() { return value.length(); }

    public byte[] current() { 
    byte [] val = new byte[value.length()];
    for(int i =0; i<value.length(); i++)
    {
        val[i] = (byte) value.get(i);
    }
    return val;}

    public boolean swap(int i, int j) {
        int ival = value.get(i);
        int jval = value.get(j);
        if ( ival <= 0 ||  jval >= maxval) {
            return false;
        }
        value.set(i, ival-1);
        value.set(j, jval+1);
        return true;
    }
}