import h5py

f = h5py.File('test_numbers.h5', 'w')

values = [
  1, 
  2**31 - 1,
  2**31 + 2, # 2147483650 doesn't fit to int32
  2**53 - 1, # 9007199254740991 fits into double without precision loss
  2**63 - 1, # 9223372036854775807
  2**63 + 2] # 9223372036854775810

ds = f.create_dataset('d_u32', dtype='u4', shape=(4,))
# note 2nd value is 32-bit unsigned int equal to 2147483650
ds[:] = values[0:4]

ds = f.create_dataset('d_i64', dtype='i8', shape=(5,))
# note 3rd value is 64-bit is equal to 9007199254740991 and
# can be stored in double without precision loss
ds[:] = values[0:5]

ds = f.create_dataset('d_u64', dtype='u8', shape=(6,))
# note 3rd value is 64-bit unsigned int equal to 9223372036854775811
ds[:] = values[0:6]

f.close() 
