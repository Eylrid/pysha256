class BitString:
    '''class for representing and handling a string of bits'''
    def __init__(self, bits):
        intbits = [int(i) for i in bits]
        if any([i not in [0,1] for i in intbits]):
            raise ValueError('all bits must be 0 or 1')
        self.bits = intbits

    def __repr__(self):
        return "<BitString: %s>" %''.join([str(i) for i in self.bits])

    def __getitem__(self, key):
        if isinstance(key, slice):
            return BitString(self.bits[key])
        else:
            return self.bits[key]

    def __len__(self):
        return len(self.bits)

    def converttoint(self):
        '''return the value of self converted to an integer'''
        n = 0
        for bit in self.bits:
            n *= 2
            n += bit
        return n

    def converttostring(self, endian='big'):
        '''return the value of self converted to a string'''
        if len(self)%8 != 0:
            raise LengthError('bitstring length not a multiple of 8.')

        s=''
        for i in range(0,len(self),8):
            byte = self[i:i+8]
            n = byte.converttoint()
            char = chr(n)
            if endian == 'big':
                s += char
            elif endian == 'little':
                s = char + s
            else:
                raise ValueError("Unrecognized endian. Must be 'big' or 'little'")

        return s

    def converttohex(self, endian='big'):
        '''return the value of self converted to hex'''
        return self.converttostring(endian).encode('hex')

    def add(self, other):
        '''add other to self, modulo 2**len(self)'''
        if not isinstance(other, BitString):
            raise TypeError('A bitstring can only be added to another bitstring.')

        sn = self.converttoint()
        on = other.converttoint()
        n = (sn + on)%(2**len(self))

        return int_to_bitstring(n, len(self))

    def copy(self):
        '''return a new bitstring with the same bits'''
        return BitString(self.bits[:])

    def extend(self, other):
        '''append the bits of other to the end of the bits of self'''
        if not isinstance(other, BitString):
            raise TypeError('A bitstring can only be extended with another bitstring.')

        self.bits.extend(other.bits)

    def prepend(self, other):
        '''prepend the bits of other to the beginning of the bits of self'''
        if not isinstance(other, BitString):
            raise TypeError('A bitstring can only be prepended with another bitstring.')

        self.bits[:0] = other.bits

    def rightrotate(self, n):
        '''
        take n bits off of the right and place them on the left
        shifting the other bits n places
        returns a new bitstring
        '''
        bits = self.bits[-n:]+self.bits[:-n]
        return BitString(bits)

    def rightshift(self, n):
        '''
        shift bits to the right n places
        vacated bits on the left become zeros
        returns a new bitstring
        '''
        bits = [0]*n+self.bits[:-n]
        return BitString(bits)

    def xor(self, other):
        '''perform a bitwise xor and return a new bitstring'''
        if not isinstance(other, BitString):
            raise TypeError('A bitstring can only be xored with another bitstring.')

        if len(other) != len(self):
            raise ValueError('bitstrings not of same length')

        newbits = []
        for sb, ob in zip(self.bits, other.bits):
            if sb == ob:
                nb = 0
            else:
                nb = 1
            newbits.append(nb)

        return BitString(newbits)

    def bit_and(self, other):
        '''perform a bitwise and and return a new bitstring'''
        if not isinstance(other, BitString):
            raise TypeError('A bitstring can only be anded with another bitstring.')

        if len(other) != len(self):
            raise ValueError('bitstrings not of same length')

        newbits = []
        for sb, ob in zip(self.bits, other.bits):
            if sb and ob:
                nb = 1
            else:
                nb = 0
            newbits.append(nb)

        return BitString(newbits)

    def bit_not(self):
        '''perform a bitwise not and return a new bitstring'''
        newbits = []
        for bit in self.bits:
            if bit:
                bit = 0
            else:
                bit = 1
            newbits.append(bit)

        return BitString(newbits)


def int_to_bitstring(n, length):
    '''take an integer and a length in bits and return a bitstring'''
    if n < 0 or n >= 2**length:
        raise ValueError('Integer out of range for length of bitstring.')

    bits = bin(n)[2:]
    bits = bits.rjust(length, '0')
    bits = [int(i) for i in bits]
    return BitString(bits)

def string_to_bitstring(s):
    '''take a string and return a bitstring'''
    result = BitString([])
    for c in s:
        result.extend(int_to_bitstring(ord(c), 8))
    return result

def padding_length(bitstring_length):
    '''
    return the number of bits of padding needed to bring
    the message length up to 448%512 in bits.
    '''
    padding_length = 448 - (bitstring_length % 512)
    if padding_length < 0:
        padding_length += 512

    return padding_length

def preprocess(bitstring):
    '''add padding and length so that bitstring is in proper format for hashing'''
    bitstring = bitstring.copy()
    bitstring_length = len(bitstring)

    #append the bit '1' to the message
    bitstring.extend(BitString([1]))

    #append k bits '0', where k is the minimum number >= 0
    #such that the resulting message length
    #(modulo 512 in bits) is 448.
    padding = BitString([0]*padding_length(len(bitstring)))
    bitstring.extend(padding)
    assert len(bitstring)% 512 == 448

    #append length of message (without the '1' bit or padding),
    #in bits, as 64-bit big-endian integer
    #(this will make the entire post-processed length a
    #multiple of 512 bits)
    length_bitstring = int_to_bitstring(bitstring_length, 64)
    bitstring.extend(length_bitstring)
    assert len(bitstring) % 512 == 0

    return bitstring

def process(bitstring):
    '''take a preprocessed bitstring and return a hash bitstring'''
    if len(bitstring)%512 != 0:
        raise LengthError('bitstring length not a multiple of 512.')

    #Initialize hash values:
    #(first 32 bits of the fractional parts of the square roots of the first 8 primes 2..19):
    h0 = int_to_bitstring(0x6a09e667, 32)
    h1 = int_to_bitstring(0xbb67ae85, 32)
    h2 = int_to_bitstring(0x3c6ef372, 32)
    h3 = int_to_bitstring(0xa54ff53a, 32)
    h4 = int_to_bitstring(0x510e527f, 32)
    h5 = int_to_bitstring(0x9b05688c, 32)
    h6 = int_to_bitstring(0x1f83d9ab, 32)
    h7 = int_to_bitstring(0x5be0cd19, 32)

    #Initialize array of round constants:
    #(first 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311):
    k = [int_to_bitstring(i, 32) for i in [
       0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
       0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
       0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
       0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
       0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
       0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
       0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
       0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]]

    chunks = [bitstring[i:i+512] for i in range(0, len(bitstring), 512)]
    for chunk in chunks:
        words = [chunk[i:i+32] for i in range(0, 512, 32)]
        w = words+[int_to_bitstring(0,32) for i in range(48)]

        #Extend the first 16 words into the remaining 48 words w[16..63] of the message schedule array:
        for i in range(16,64):
            s0 = w[i-15].rightrotate(7).xor(w[i-15].rightrotate(18)).xor(w[i-15].rightshift(3))
            s1 = w[i-2].rightrotate(17).xor(w[i-2].rightrotate(19)).xor(w[i-2].rightshift(10))
            w[i] = w[i-16].add(s0).add(w[i-7]).add(s1)

        #Initialize working variables to current hash value:
        a = h0
        b = h1
        c = h2
        d = h3
        e = h4
        f = h5
        g = h6
        h = h7

        #Compression function main loop:
        for i in range(64):
            S1 = e.rightrotate(6).xor(e.rightrotate(11)).xor(e.rightrotate(25))
            ch = e.bit_and(f).xor(e.bit_not().bit_and(g)) ##
            temp1 = h.add(S1).add(ch).add(k[i]).add(w[i])
            S0 = a.rightrotate(2).xor(a.rightrotate(13)).xor(a.rightrotate(22))
            maj = a.bit_and(b).xor(a.bit_and(c)).xor(b.bit_and(c))
            temp2 = S0.add(maj)

            h = g
            g = f
            f = e
            e = d.add(temp1)
            d = c
            c = b
            b = a
            a = temp1.add(temp2)

        #Add the compressed chunk to the current hash value:
        h0 = h0.add(a)
        h1 = h1.add(b)
        h2 = h2.add(c)
        h3 = h3.add(d)
        h4 = h4.add(e)
        h5 = h5.add(f)
        h6 = h6.add(g)
        h7 = h7.add(h)

    #Produce the final hash value (big-endian):
    digest = BitString([])
    digest.extend(h0)
    digest.extend(h1)
    digest.extend(h2)
    digest.extend(h3)
    digest.extend(h4)
    digest.extend(h5)
    digest.extend(h6)
    digest.extend(h7)

    return digest

def hash(message, hexresult=False, endian='big'):
    '''take a string and return a hash'''
    bitstring = string_to_bitstring(message)
    bitstring = preprocess(bitstring)
    hash = process(bitstring)
    if hexresult:
        return hash.converttohex(endian)
    else:
        return hash.converttostring(endian)

def bitcoin_hash(message):
    '''does two rounds and returns the result as hex'''
    return hash(hash(message), hexresult=True, endian='little')

def benchmark(rounds=100, version='\x02\x00\x00\x00', prev_block='\x00'*32,
              merkle_root='\x00'*32, timestamp='\x00'*4,
              bits='\xff\xff\x00\x1d'):
    '''returns the number of hashes per second'''
    import time
    t1=time.time()
    for i in range(rounds):
        nonce = int_to_bitstring(i,32).converttostring(endian='little')
        header = version+prev_block+merkle_root+timestamp+bits+nonce
        bitcoin_hash(str(i))
    t2=time.time()
    return rounds/(t2-t1)
