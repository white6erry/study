import tensorflow as tf
sess = tf.Session()

t = [1, 2, 3, 4, 5, 6]
t = tf.reshape(t, [1, 2, 1, 3, 1, 1])

print(sess.run(t))

input  = [[[1, 1, 1], [2, 2, 2]], [[3, 3, 3], [4, 4, 4]], [[5, 5, 5],[6 ,6, 6]]]
print(sess.run(tf.slice(input, [1, 0, 0], [1, 1, 3]))) 
